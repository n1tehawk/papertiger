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
  ISO8601FullDateFormat = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"'; //Format string used to go to/from ISO8601 dates

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
  public
    // Returns path+filename for requested image - ImageNumber gives the order
    function ImagePath(DocumentID: integer; ImageNumber: integer=1): string;
    // Inserts a new image record in database (specify image number/sequence >1 to place image after existing images for a document)
    // Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
    // Returns image ID.
    function InsertImage(const DocumentID, Sequence: integer; const Path, ImageHash: string): integer;
    // Inserts a new scan record in database; retruns scan ID. Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
    function InsertDocument(const DocumentName, PDFPath, DocumentHash: string; TheScanDate: TDateTime): integer;
    // Lists document with DocumentID or all documents if DocumentID=DBINVALIDID
    procedure ListDocuments(const DocumentID: integer; var DocumentsArray: TJSONArray);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  dbconfig;

const
  SettingsFile = 'tigerserver.ini';


{ TTigerDB }

function TTigerDB.ImagePath(DocumentID: integer; ImageNumber: integer=1): string;
begin
  result:='';
  if DocumentID = INVALIDID then
  begin
    TigerLog.WriteLog(etWarning, 'ImagePath: invalid document ID requested. Cannot find image filename.');
    exit;
  end;

  if FReadTransaction.Active = false then
    FReadTransaction.StartTransaction;
  try
    FReadQuery.SQL.Text := 'SELECT PATH FROM IMAGES WHERE ID=' + IntToStr(DocumentID) +' AND SEQUENCE='+inttostr(ImageNumber);
    FReadQuery.Open;
    if not(FReadQuery.EOF) then
      result:=FReadQuery.FieldByName('PATH').AsString;
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

function TTigerDB.InsertImage(const DocumentID, Sequence: integer; const Path, ImageHash: string): integer;
begin
  //todo: make this function insert or update image so it can modify existing records
  Result := INVALIDID;
  try
    if FReadWriteTransaction.Active = false then
      FReadWriteTransaction.StartTransaction;
    FInsertImage.Close;
    FInsertImage.ParamByName('DOCUMENTID').AsInteger := DocumentID;
    if Sequence=0 then
      raise Exception.Create('Sequence number 0 is not allowed. Please specify 1 or higher.');
    FInsertImage.ParamByName('SEQUENCE').AsInteger := Sequence;
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
      TigerLog.WriteLog(etError, 'Database error: ' + E.Message, true);
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'Exception: ' + F.ClassName + '/' + F.Message, true);
    end;
  end;
end;

function TTigerDB.InsertDocument(const DocumentName, PDFPath, DocumentHash: string; TheScanDate: TDateTime): integer;
begin
  Result := INVALIDID;
  try
    if FReadWriteTransaction.Active = false then
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
      TigerLog.WriteLog(etError, 'InsertDocument: Database error: ' + E.Message, true);
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      TigerLog.WriteLog(etError, 'InsertDocument: Exception: ' + F.Message, true);
    end;
  end;
end;

procedure TTigerDB.ListDocuments(const DocumentID: integer; var DocumentsArray: TJSONArray);
// Will return an array containing objects/records for each document
var
  RecordObject: TJSONObject;
begin
  //todo: convert to generic db query => json array function
  DocumentsArray:=TJSONArray.Create; //clears any existing data at the same time
  if FReadTransaction.Active = false then
    FReadTransaction.StartTransaction;
  try
    if DocumentID = INVALIDID then
      // All documents
      FReadQuery.SQL.Text := 'SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS'
    else
      // Specified document; no need for parametrized queries: one time only, integer
      FReadQuery.SQL.Text := 'SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS WHERE ID=' + IntToStr(DocumentID);
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
      RecordObject.Add('scandate', FormatDateTime(ISO8601FullDateFormat,FReadQuery.FieldByName('SCANDATE').AsDateTime));
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
      DocumentsArray.Add(TJSONString.Create('ListDocuments: db exception: ' + E.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: db exception: ' + E.Message);
      FReadTransaction.RollBack;
    end;
    on F: Exception do
    begin
      DocumentsArray.Clear;
      DocumentsArray.Add(TJSONString.Create('ListDocuments: exception: message ' + F.Message));
      TigerLog.WriteLog(etError, 'ListDocuments: exception: ' + F.Message);
    end;
  end;
end;

constructor TTigerDB.Create;
var
  Settings: TDBConnectionConfig;
  SQL: string;
begin
  inherited Create;
  Settings := TDBConnectionConfig.Create('Firebird', '', 'tiger.fdb', 'SYSDBA', 'masterkey', 'UTF8');
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
        TigerLog.WriteLog(etWarning, 'Warning: unknown database type ' + Settings.DBType + ' specified. Defaulting to Firebird', true);
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

    // Check for existing database
    if (FDB.HostName = '') and (FileExists(FDB.DatabaseName) = false) then
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
          TigerLog.WriteLog(etDebug, 'Tried to create database ' + FDB.DatabaseName + ' but could not.', true);
          raise Exception.CreateFmt('Tried to create database %s but could not.', [FDB.DatabaseName]);
        end;
      end;
    end;
  finally
    Settings.Free;
  end;

  FDB.Open;
  if not (FDB.Connected) then
  begin
    TigerLog.WriteLog(etDebug, 'Error opening database ' + FDB.DatabaseName, true);
    raise Exception.CreateFmt('Error opening databas %s.', [FDB.DatabaseName]);
  end;

  // Get transactions linked to the right database connection:
  FDB.Transaction := FReadWriteTransaction; //Default transaction for database
  FReadTransaction.Database := FDB;

  // todo: Check for+create required tables
  FInsertImage.Database := FDB;
  FInsertImage.Transaction := FReadWriteTransaction;
  //Try to work around FPC 2.6.0 bug that doesn't do Open, but execute for INSERT statements
  FInsertImage.ParseSQL := false;
  //todo: replace with merge/insert replacing
  SQL := 'INSERT INTO IMAGES (DOCUMENTID,SEQUENCE,PATH,IMAGEHASH) ' + 'VALUES (:DOCUMENTID,:SEQUENCE,:PATH,:IMAGEHASH) RETURNING ID';
  FInsertImage.SQL.Text := SQL;
  FInsertImage.Prepare;

  FInsertScan.Database := FDB;
  FInsertScan.Transaction := FReadWriteTransaction;
  FInsertScan.ParseSQL := false;
  SQL := 'INSERT INTO DOCUMENTS (DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH) ' +
    'VALUES (:DOCUMENTNAME,:PDFPATH,:SCANDATE,:DOCUMENTHASH) RETURNING ID';
  FInsertScan.SQL.Text := SQL;
  FInsertScan.Prepare;

  FReadQuery.Database := FDB;
  FReadQuery.Transaction := FReadTransaction;
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
    FDB.Connected := false;
  FInsertImage.Free;
  FInsertScan.Free;
  FReadQuery.Free;
  FReadWriteTransaction.Free;
  FReadTransaction.Free;
  FDB.Free;
  inherited Destroy;
end;

end.



