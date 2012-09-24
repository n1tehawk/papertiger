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
  FInsertImage: TSQLQuery; //Inserts new images
  FInsertScan: TSQLQuery; //Inserts new scan data.
  FReadTransaction: TSQLTransaction; //Transaction for read-only access
  FReadWriteTransaction: TSQLTransaction; //Transaction for read/write access
public
  function InsertImage(const DocumentID: integer; const Path, ImageHash: string):integer; //Inserts a new image record in database; returns image ID. Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
  function InsertDocument(const DocumentName, PDFPath, DocumentHash: string; TheScanDate: TDateTime):integer; //Insterts a new scan record in database; retruns scan ID. Keep string values empty to insert NULLs; pass a pre 1900 date for TheScanDate to do the same.
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

constructor TTigerDB.Create;
var
  Settings: TDBConnectionConfig;
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
  FInsertImage.SQL.Text := 'INSERT INTO IMAGES (DOCUMENTID,PATH,IMAGEHASH) '
    +'VALUES (:DOCUMENTID,:PATH,:IMAGEHASH) RETURNING ID';
  FInsertImage.Prepare;

  FInsertScan.Database := FDB;
  FInsertScan.Transaction := FReadWriteTransaction;
  FInsertScan.SQL.Text := 'INSERT INTO DOCUMENTS (DOCUMENTNAME,PDFPATH,SCANDATE,DOCUMENTHASH) '
    +'VALUES (:DOCUMENTNAME,:PDFPATH,:SCANDATE,:DOCUMENTHASH) RETURNING ID';
  FInsertScan.Prepare;
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
  FReadWriteTransaction.Free;
  FReadTransaction.Free;
  FDB.Free;
  inherited Destroy;
end;

end.

