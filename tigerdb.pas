unit tigerdb;

{ Database connection for papertiger.
  Currently supports Firebird, sqlite

  Copyright (c) 2012-2013 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
{
For complex, array-like functions, use JSON objects etc. They can be easily passed on via the CGI server layer
and the regular server application can decode them to text.
}



{$i tigerserver.inc}

interface

uses
  Classes, SysUtils,
  tigerutil {for logging},
  sqldb,
  DB {for EDatabaseError},
  ibconnection {Firebird},
  pqconnection {PostgreSQL},
  sqlite3conn {SQLite},
  fpjson,
  dateutils;

{$i tigercommondefs.inc}

const
  ISO8601FullDateFormat = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"';
//Format string used to go to/from ISO8601 dates

type
  { TTigerDB }

  TTigerDB = class(TObject)
  private
    FDB: TSQLConnection; //Database connection
    FInsertImage: TSQLQuery; //Inserts new images (for now with an insert query)
    FInsertScan: TSQLQuery; //Inserts new scan data.
    FReadQuery: TSQLQuery; //Query used for reading data
    FReadTransaction: TSQLTransaction; //Transaction for read-only access
    FReadWriteTransaction: TSQLTransaction; //Transaction for read/write access
    FWriteQuery: TSQLQuery; //Query used for writing misc data.
  public
    // Returns highest existing imageorder for images or 0 if error
    function GetHighestImageOrder(DocumentID: integer): integer;
    // Returns path+filename for requested image - imageorder gives the sort order/image number
    function ImagePath(DocumentID: integer; Imageorder: integer = 1): string;
    // Inserts a new scan record in database; retruns scan ID.
    // Keep string values empty to insert NULLs;
    // TheScanDate: please pass UTC date/time, pass a pre 1900 date to specify unknown date
    function InsertDocument(const DocumentName, PDFPath, DocumentHash: string;
      TheScanDate: TDateTime): integer;
    // Inserts a new image record in database (specify specifc image order or -1 to place image after existing images for a document)
    // Returns image ID.
    function InsertImage(const DocumentID, Imageorder: integer;
      const Path, ImageHash: string): integer;
    // Lists document with DocumentID or all documents if DocumentID=INVALIDID
    procedure ListDocuments(const DocumentID: integer; var DocumentsArray: TJSONArray);
    // List images with DocumentID or all images if DocumentID=INVALIDID. Path has full path and image filename.
    procedure ListImages(const DocumentID: integer; var DocumentsArray: TJSONArray);
    // Returns path+filename for PDF associated with document
    function GetPDFPath(DocumentID: integer): string;
    // Sets path+filename for PDF associated with document. Returns result.
    function SetPDFPath(DocumentID: integer; PDFPath: string): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  dbconfig;

const
  SettingsFile = 'tigerserver.ini';


{ TTigerDB }

function TTigerDB.GetHighestImageOrder(DocumentID: integer): integer;
begin
  Result := 0;
  if DocumentID = INVALIDID then
  begin
    TigerLog.WriteLog(etWarning, 'GetHighestImageOrder: invalid document ID requested.');
    exit;
  end;

  if FReadTransaction.Active = False then
    FReadTransaction.StartTransaction;
  try
    // Select top imageorder number; I just don't want to understand GROUP BY and this works.
    FReadQuery.SQL.Text := 'SELECT IMAGEORDER FROM IMAGES WHERE DOCUMENTID=' +
      IntToStr(DocumentID) + ' ORDER BY IMAGEORDER DESC ROWS 1';
    FReadQuery.Open;
    if not (FReadQuery.EOF) then
      Result := FReadQuery.FieldByName('IMAGEORDER').AsInteger;
    FReadQuery.Close;
    FReadTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      TigerLog.WriteLog(etError, 'GetHighestImageOrder: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      TigerLog.WriteLog(etError, 'GetHighestImageOrder: exception: ' + F.Message);
    end;
  end;
end;

function TTigerDB.ImagePath(DocumentID: integer; Imageorder: integer = 1): string;
begin
  Result := '';
  if DocumentID = INVALIDID then
  begin
    TigerLog.WriteLog(etWarning,
      'ImagePath: invalid document ID requested. Cannot find image filename.');
    exit;
  end;

  if FReadTransaction.Active = False then
    FReadTransaction.StartTransaction;
  try
    FReadQuery.SQL.Text := 'SELECT PATH FROM IMAGES WHERE DOCUMENTID=' +
      IntToStr(DocumentID) + ' AND IMAGEORDER=' + IntToStr(ImageOrder);
    FReadQuery.Open;
    if not (FReadQuery.EOF) then
      Result := FReadQuery.FieldByName('PATH').AsString;
    FReadQuery.Close;
    FReadTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      TigerLog.WriteLog(etError, 'ImagePath: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      TigerLog.WriteLog(etError, 'ImagePath: exception: ' + F.Message);
    end;
  end;
end;

function TTigerDB.InsertImage(const DocumentID, Imageorder: integer;
  const Path, ImageHash: string): integer;
var
  NewImageOrder: integer;
begin
  //todo: make this function insert or update image so it can modify existing records
  Result := INVALIDID;
  try
    if ImageOrder <= 0 then
    begin
      // get next image order number
      NewImageOrder := GetHighestImageOrder(DocumentID);
      if NewImageOrder = INVALIDID then
      begin
        TigerLog.WriteLog(etError,
          'InsertImage: could not get new image order for document: ' +
          IntToStr(DocumentID), True);
        exit;
      end;
      NewImageOrder := NewImageOrder + 1;
    end
    else
    begin
      NewImageOrder := ImageOrder;
    end;
    if FReadWriteTransaction.Active = False then
      FReadWriteTransaction.StartTransaction;
    FInsertImage.Close;
    FInsertImage.ParamByName('DOCUMENTID').AsInteger := DocumentID;
    FInsertImage.ParamByName('ImageOrder').AsInteger := NewImageOrder;
    if Path = '' then // NULL
      FInsertImage.ParamByName('PATH').Clear
    else
      FInsertImage.ParamByName('PATH').AsString := Path;
    if ImageHash = '' then // NULL
      FInsertImage.ParamByName('IMAGEHASH').Clear
    else
      FInsertImage.ParamByName('IMAGEHASH').AsString := ImageHash;
    FInsertImage.Open;
    if not (FInsertImage.EOF) then
      Result := FInsertImage.Fields[0].AsInteger;
    FInsertImage.Close;
    FReadWriteTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'Database error: ' + E.Message, True);
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'Exception: ' + F.ClassName + '/' + F.Message, True);
    end;
  end;
end;

function TTigerDB.InsertDocument(const DocumentName, PDFPath, DocumentHash: string;
  TheScanDate: TDateTime): integer;
begin
  Result := INVALIDID;
  try
    if FReadWriteTransaction.Active = False then
      FReadWriteTransaction.StartTransaction;
    FInsertScan.Close;
    if DocumentName = '' then // NULL
      FInsertScan.ParamByName('DOCUMENTNAME').Clear
    else
      FInsertScan.ParamByName('DOCUMENTNAME').AsString := DocumentName;
    if PDFPath = '' then // NULL
      FInsertScan.ParamByName('PDFPATH').Clear
    else
      FInsertScan.ParamByName('PDFPATH').AsString := PDFPath;
    if DocumentHash = '' then // NULL
      FInsertScan.ParamByName('DOCUMENTHASH').Clear
    else
      FInsertScan.ParamByName('DOCUMENTHASH').AsString := DocumentHash;
    // Scan dates before say 1900 must be fake
    if TheScanDate <= EncodeDate(1900, 1, 1) then
      // NULL
      FInsertScan.ParamByName('SCANDATE').Clear
    else
      FInsertScan.ParamByName('SCANDATE').AsDateTime := TheScanDate;
    FInsertScan.Open;
    if not (FInsertScan.EOF) then
      Result := FInsertScan.Fields[0].AsInteger;
    FInsertScan.Close;
    FReadWriteTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'InsertDocument: Database error: ' + E.Message, True);
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'InsertDocument: Exception: ' + F.Message, True);
    end;
  end;
end;

procedure TTigerDB.ListDocuments(const DocumentID: integer;
  var DocumentsArray: TJSONArray);
// Will return an array containing objects/records for each document
var
  RecordObject: TJSONObject;
begin
  //todo: convert to generic db query => json array function
  DocumentsArray := TJSONArray.Create; //clears any existing data at the same time
  if FReadTransaction.Active = False then
    FReadTransaction.StartTransaction;
  try
    if DocumentID = INVALIDID then
      // All documents
      FReadQuery.SQL.Text :=
        'SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS ORDER BY ID'
    else
      // Specified document; no need for parametrized queries: one time only, integer
      FReadQuery.SQL.Text :=
        'SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS WHERE ID=' +
        IntToStr(DocumentID);
    FReadQuery.Open;
    while not FReadQuery.EOF do
    begin
      RecordObject := TJSONObject.Create();
      RecordObject.Add('id', FReadQuery.FieldByName('ID').AsInteger);
      RecordObject.Add('documentname', FReadQuery.FieldByName('DOCUMENTNAME').AsString);
      RecordObject.Add('pdfpath', FReadQuery.FieldByName('PDFPATH').AsString);
      // Convert dates to ISO 8601 UTC-based date/times e.g.:
      // 2013-02-21T09:47:42.467Z
      // Assuming the dates stored in the DB are UTC.
      RecordObject.Add('scandate', FormatDateTime(ISO8601FullDateFormat,
        FReadQuery.FieldByName('SCANDATE').AsDateTime));
      RecordObject.Add('documenthash', FReadQuery.FieldByName('DOCUMENTHASH').AsString);
      DocumentsArray.Add(RecordObject);
      FReadQuery.Next;
    end;
    FReadQuery.Close;
    FReadTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      DocumentsArray.Clear;
      DocumentsArray.Add(TJSONString.Create('ListDocuments: db exception: ' +
        E.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      DocumentsArray.Clear;
      DocumentsArray.Add(TJSONString.Create('ListDocuments: exception: message ' +
        F.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: exception: ' + F.Message);
    end;
  end;
end;

procedure TTigerDB.ListImages(const DocumentID: integer; var DocumentsArray: TJSONArray);
// Will return an array containing objects/records for each image
var
  RecordObject: TJSONObject;
begin
  //todo: convert to generic db query => json array function
  DocumentsArray := TJSONArray.Create; //clears any existing data at the same time
  if FReadTransaction.Active = False then
    FReadTransaction.StartTransaction;
  try
    if DocumentID = INVALIDID then
      // Images for all documents
      FReadQuery.SQL.Text :=
        'SELECT ID,IMAGEORDER,DOCUMENTID,PATH,IMAGEHASH FROM IMAGES ORDER BY DOCUMENTID,IMAGEORDER '
    else
      // Specified document; no need for parameterized queries: one time only, integer
      FReadQuery.SQL.Text :=
        'SELECT ID,IMAGEORDER,DOCUMENTID,PATH,IMAGEHASH FROM IMAGES WHERE DOCUMENTID=' +
        IntToStr(DocumentID) + ' ORDER BY IMAGEORDER ';
    FReadQuery.Open;
    while not FReadQuery.EOF do
    begin
      RecordObject := TJSONObject.Create();
      RecordObject.Add('id', FReadQuery.FieldByName('ID').AsInteger);
      RecordObject.Add('imageorder', FReadQuery.FieldByName('IMAGEORDER').AsInteger);
      RecordObject.Add('documentid', FReadQuery.FieldByName('DOCUMENTID').AsInteger);
      RecordObject.Add('path', FReadQuery.FieldByName('PATH').AsString);
      RecordObject.Add('imagehash', FReadQuery.FieldByName('IMAGEHASH').AsString);
      DocumentsArray.Add(RecordObject);
      FReadQuery.Next;
    end;
    FReadQuery.Close;
    FReadTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      DocumentsArray.Clear;
      DocumentsArray.Add(TJSONString.Create('ListImages: db exception: ' + E.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      DocumentsArray.Clear;
      DocumentsArray.Add(TJSONString.Create('ListImages: exception: message ' +
        F.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: exception: ' + F.Message);
    end;
  end;
end;

function TTigerDB.GetPDFPath(DocumentID: integer): string;
begin
  Result := '';
  if DocumentID = INVALIDID then
  begin
    TigerLog.WriteLog(etWarning,
      'PDFPath: invalid document ID requested. Cannot find PDF filename.');
    exit;
  end;

  if FReadTransaction.Active = False then
    FReadTransaction.StartTransaction;
  try
    FReadQuery.SQL.Text := 'SELECT PDFPATH FROM DOCUMENTS WHERE ID=' +
      IntToStr(DocumentID);
    FReadQuery.Open;
    if not (FReadQuery.EOF) then
      Result := FReadQuery.FieldByName('PDFPATH').AsString;
    FReadQuery.Close;
    FReadTransaction.Commit;
  except
    on E: EDatabaseError do
    begin
      TigerLog.WriteLog(etError, 'PDFPath: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      TigerLog.WriteLog(etError, 'PDFPath: exception: ' + F.Message);
    end;
  end;
end;

function TTigerDB.SetPDFPath(DocumentID: integer; PDFPath: string): boolean;
begin
  Result := False;
  try
    if FReadWriteTransaction.Active = False then
      FReadWriteTransaction.StartTransaction;
    FWriteQuery.Close;
    FWriteQuery.SQL.Text := 'UPDATE DOCUMENTS SET PDFPATH=:PDFPATH WHERE ID=' +
      IntToStr(DocumentID);

    if PDFPath = '' then // NULL
      FWriteQuery.ParamByName('PDFPATH').Clear
    else
      FWriteQuery.ParamByName('PDFPATH').AsString := PDFPath;
    FWriteQuery.ExecSQL;
    FWriteQuery.Close;
    FReadWriteTransaction.Commit;
    Result := True;
  except
    on E: EDatabaseError do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'SetPDFPath: Database error: ' + E.Message, True);
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'SetPDFPath: Exception: ' + F.Message, True);
    end;
  end;
end;

constructor TTigerDB.Create;
var
  Settings: TDBConnectionConfig;
  SQL: string;
begin
  inherited Create;
  Settings := TDBConnectionConfig.Create('Firebird', '', 'tiger.fdb',
    'SYSDBA', 'masterkey', 'UTF8');
  try
    Settings.SettingsFile := SettingsFile;
    // Set up db connection
    case Settings.DBType of
      'Firebird':
      begin
        FDB := TIBConnection.Create(nil);
        TIBConnection(FDB).Dialect := 3; //just to be sure
      end;
      'PostgreSQL': FDB := TPQConnection.Create(nil);
      'SQLite': FDB := TSQLite3Connection.Create(nil);
      else
      begin
        TigerLog.WriteLog(etWarning, 'Warning: unknown database type ' +
          Settings.DBType + ' specified. Defaulting to Firebird', True);
        FDB := TIBConnection.Create(nil);
        TIBConnection(FDB).Dialect := 3; //just to be sure
      end;
    end;

    FDB.HostName := Settings.DBHost;
    FDB.DatabaseName := Settings.DBPath;
    FDB.UserName := Settings.DBUser;
    FDB.Password := Settings.DBPassword;
    FDB.CharSet := Settings.DBCharset;

    FReadWriteTransaction := TSQLTransaction.Create(nil);
    FReadTransaction := TSQLTransaction.Create(nil);
    FInsertImage := TSQLQuery.Create(nil);
    FInsertScan := TSQLQuery.Create(nil);
    FReadQuery := TSQLQuery.Create(nil);
    FWriteQuery := TSQLQuery.Create(nil);

    // Check for existing database
    if (FDB.HostName = '') and (FileExists(FDB.DatabaseName) = False) then
    begin
      if (FDB is TIBConnection) or (FDB is TSQLite3Connection) then
      begin
        if (FDB is TIBConnection) then
          TIBConnection(FDB).CreateDB;
        if (FDB is TSQLite3Connection) then
          TSQLite3Connection(FDB).CreateDB;
        sleep(10);
        if not (FileExists(FDB.DatabaseName)) then
        begin
          TigerLog.WriteLog(etDebug, 'Tried to create database ' +
            FDB.DatabaseName + ' but could not.', True);
          raise Exception.CreateFmt('Tried to create database %s but could not.',
            [FDB.DatabaseName]);
        end;
      end;
    end;
  finally
    Settings.Free;
  end;

  FDB.Open;
  if not (FDB.Connected) then
  begin
    TigerLog.WriteLog(etDebug, 'Error opening database ' + FDB.DatabaseName, True);
    raise Exception.CreateFmt('Error opening databas %s.', [FDB.DatabaseName]);
  end;

  // Get transactions linked to the right database connection:
  FDB.Transaction := FReadWriteTransaction; //Default transaction for database
  FReadTransaction.Database := FDB;

  // todo: Check for+create required tables
  FInsertImage.Database := FDB;
  FInsertImage.Transaction := FReadWriteTransaction;
  //Try to work around FPC 2.6.0 bug that doesn't do Open, but execute for INSERT statements
  FInsertImage.ParseSQL := False;
  //todo: replace with merge/insert replacing
  SQL := 'INSERT INTO IMAGES (DOCUMENTID,IMAGEORDER,PATH,IMAGEHASH) ' +
    'VALUES (:DOCUMENTID,:IMAGEORDER,:PATH,:IMAGEHASH) RETURNING ID';
  FInsertImage.SQL.Text := SQL;
  FInsertImage.Prepare;

  FInsertScan.Database := FDB;
  FInsertScan.Transaction := FReadWriteTransaction;
  FInsertScan.ParseSQL := False;
  SQL := 'INSERT INTO DOCUMENTS (DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH) ' +
    'VALUES (:DOCUMENTNAME,:PDFPATH,:SCANDATE,:DOCUMENTHASH) RETURNING ID';
  FInsertScan.SQL.Text := SQL;
  FInsertScan.Prepare;

  FReadQuery.Database := FDB;
  FReadQuery.Transaction := FReadTransaction;

  FWriteQuery.Database := FDB;
  FWriteQuery.Transaction := FReadWriteTransaction;
end;

destructor TTigerDB.Destroy;
begin
  if Assigned(FInsertImage) then
    FInsertScan.Close;
  if Assigned(FInsertScan) then
    FInsertScan.Close;
  if Assigned(FReadWriteTransaction) then
    FReadWriteTransaction.Rollback;
  if Assigned(FReadTransaction) then
    FReadTransaction.Rollback;
  if Assigned(FDB) then
    FDB.Connected := False;
  FInsertImage.Free;
  FInsertScan.Free;
  FReadQuery.Free;
  FWriteQuery.Free;
  FReadWriteTransaction.Free;
  FReadTransaction.Free;
  FDB.Free;
  inherited Destroy;
end;

end.
