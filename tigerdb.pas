unit tigerdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  sqldb,
  ibconnection {Firebird},
  pqconnection {PostgreSQL},
  sqlite3conn {SQLite};

const
  DBINVALIDID=-1; //Used to return invalid primary key ids for db objects
type
{ TTigerDB }

TTigerDB= class(TObject)
private
  FDB: TSQLConnection; //Database connection
  FInsertImage: TSQLQuery; //Inserts new images (for now with an insert query)
  FInsertScan: TSQLQuery; //Inserts new scan data.
  FReadQuery: TSQLQuery; //Query used for reading data
  FReadTransaction: TSQLTransaction; //Transaction for read-only access
  FReadWriteTransaction: TSQLTransaction; //Transaction for read/write access
public
  function InsertImage(const DocumentID: integer; const Path, ImageHash: string):integer; //Inserts a new image record in database; returns image ID. Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
  function InsertDocument(const DocumentName, PDFPath, DocumentHash: string; TheScanDate: TDateTime):integer; //Insterts a new scan record in database; retruns scan ID. Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
  function ListDocuments(const DocumentID: integer): string;
  constructor Create;
  destructor Destroy; override;
end;

implementation

uses
  dbconfig, dateutils;

const
  SettingsFile = 'tigerserver.ini';


{ TTigerDB }

function TTigerDB.InsertImage(const DocumentID: integer; const Path,
  ImageHash: string): integer;
begin
  //todo: make this function insert or update image so it can modify existing records
  result:=DBINVALIDID;
  try
    if FReadWriteTransaction.Active = false then
      FReadWriteTransaction.StartTransaction;
    FInsertImage.Close;
    FInsertImage.ParamByName('DOCUMENTID').AsInteger:=DocumentID;
    if Path='' then // NULL
      FInsertImage.ParamByName('PATH').Clear
    else
      FInsertImage.ParamByName('PATH').AsString:=Path;
    if ImageHash='' then // NULL
      FInsertImage.ParamByName('IMAGEHASH').Clear
    else
      FInsertImage.ParamByName('IMAGEHASH').AsString:=ImageHash;
    FInsertImage.Open;
    if not(FInsertImage.EOF) then
      result:=FInsertImage.Fields[0].AsInteger;
    FInsertImage.Close;
    FReadWriteTransaction.Commit;
  except
    on E: EIBDatabaseError do
    begin
      if FReadWriteTransaction.Active then
      FReadWriteTransaction.Rollback;
        writeln('Database error: ' + E.Message + '(error code: ' + IntToStr(E.GDSErrorCode) + ')');
        writeln('');
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      writeln('Exception: ' + F.ClassName + '/' + F.Message);
      writeln('');
    end;
  end;
end;

function TTigerDB.InsertDocument(const DocumentName, PDFPath, DocumentHash: string;
  TheScanDate: TDateTime): integer;
begin
  result:=DBINVALIDID;
  try
    if FReadWriteTransaction.Active = false then
      FReadWriteTransaction.StartTransaction;
    FInsertScan.Close;
    if DocumentName='' then // NULL
      FInsertScan.ParamByName('DOCUMENTNAME').Clear
    else
      FInsertScan.ParamByName('DOCUMENTNAME').AsString:=DocumentName;
    if PDFPath='' then // NULL
      FInsertScan.ParamByName('PDFPATH').Clear
    else
      FInsertScan.ParamByName('PDFPATH').AsString:=PDFPath;
    if DocumentHash='' then // NULL
      FInsertScan.ParamByName('DOCUMENTHASH').Clear
    else
      FInsertScan.ParamByName('DOCUMENTHASH').AsString:=DocumentHash;
    // Scan dates before say 1900 must be fake
    if TheScanDate <= EncodeDate(1900,1,1) then
      // NULL
      FInsertScan.ParamByName('SCANDATE').Clear
    else
      FInsertScan.ParamByName('SCANDATE').AsDateTime:=TheScanDate;
    FInsertScan.Open;
    if not(FInsertScan.EOF) then
      result:=FInsertScan.Fields[0].AsInteger;
    FInsertScan.Close;
    FReadWriteTransaction.Commit;
  except
    on E: EIBDatabaseError do
    begin
      if FReadWriteTransaction.Active then
      FReadWriteTransaction.Rollback;
        writeln('Database error: ' + E.Message + '(error code: ' + IntToStr(E.GDSErrorCode) + ')');
        writeln('');
    end;
    on F: Exception do
    begin
      if FReadWriteTransaction.Active then
        FReadWriteTransaction.Rollback;
      writeln('Exception: ' + F.ClassName + '/' + F.Message);
      writeln('');
    end;
  end;
end;

function TTigerDB.ListDocuments(const DocumentID: integer): string;
begin
  if DocumentID=DBINVALIDID then
    // All documents
    FReadQuery.SQL.Text:='SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS'
  else
    // Specified document; no need for parametrized queries: one time only, integer
    FReadQuery.SQL.Text:='SELECT ID,DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH FROM DOCUMENTS WHERE ID='+inttostr(DocumentID);
  FReadTransaction.StartTransaction;
  FReadQuery.Open;
  while not FReadQuery.EOF do
  begin
    if not(FReadQuery.BOF) then result:=result+#13+#10;
    result:=result+FReadQuery.FieldByName('ID').AsString+','+
      FReadQuery.FieldByName('ID').AsString+','+
      FReadQuery.FieldByName('DOCUMENTNAME').AsString+','+
      FReadQuery.FieldByName('PDFPATH').AsString+','+
      FReadQuery.FieldByName('SCANDATE').AsString+','+
      FReadQuery.FieldByName('DOCUMENTHASH').AsString;
    FReadQuery.Next;
  end;
  FReadQuery.Close;
  FReadTransaction.Commit;
end;

constructor TTigerDB.Create;
var
  Settings: TDBConnectionConfig;
  SQL: string;
begin
  inherited Create;
  Settings:=TDBConnectionConfig.Create('Firebird','','tiger.fdb','SYSDBA','masterkey','UTF8');
  try
    Settings.SettingsFile:=SettingsFile;
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
        writeln('Warning: unknown database type ' + Settings.DBType + ' specified.');
        writeln('Defaulting to Firebird');
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
    FReadTransaction:=TSQLTransaction.Create(nil);
    FInsertImage:=TSQLQuery.Create(nil);
    FInsertScan:=TSQLQuery.Create(nil);
    FReadQuery:=TSQLQuery.Create(nil);

    // Check for existing database
    if (FDB is TIBConnection) and (FDB.HostName='') and (FileExists(FDB.DatabaseName)=false) then
      TIBConnection(FDB).CreateDB;
    if (FDB is TSQLite3Connection) and (FileExists(FDB.DatabaseName)=false) then
      TSQLite3Connection(FDB).CreateDB;
  finally
    Settings.Free;
  end;
  FDB.Open;
  FDB.Transaction := FReadWriteTransaction; //Default transaction for database

  // todo: Check for+create required tables
  FInsertImage.Database := FDB;
  FInsertImage.Transaction := FReadWriteTransaction;
  //Try to work around FPC 2.6.0 bug that doesn't do Open, but execute for INSERT statements
  FInsertImage.ParseSQL:=false;
  //todo: replace with merge/insert replacing
  SQL:='INSERT INTO IMAGES (DOCUMENTID,PATH,IMAGEHASH) '+
    'VALUES (:DOCUMENTID,:PATH,:IMAGEHASH) RETURNING ID';
  FInsertImage.SQL.Text := SQL;
  FInsertImage.Prepare;
  {$IFDEF DEBUG}
  writeln('FInsertImage:');
  writeln (FInsertImage.SQL.Text);
  {$ENDIF}

  FInsertScan.Database := FDB;
  FInsertScan.Transaction := FReadWriteTransaction;
  FInsertScan.ParseSQL:=false;
  SQL:='INSERT INTO DOCUMENTS (DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH) '+
    'VALUES (:DOCUMENTNAME,:PDFPATH,:SCANDATE,:DOCUMENTHASH) RETURNING ID';
  FInsertScan.SQL.Text:=SQL;
  FInsertScan.Prepare;
  {$IFDEF DEBUG}
  writeln('FInsertScan:');
  writeln (FInsertScan.SQL.Text);
  {$ENDIF}

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

