unit tigerutil;

{ Utility functions such as logging support.

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



{$i tigerserver.inc}
{$IFDEF MSWINDOWS}
{$R fclel.res}//needed for message files to get Windows to display event log contents correctly
// Not needed for *nix
{$ENDIF}

interface

uses
  Classes, SysUtils, eventlog;

type
  { TLogger }
  TLogger = class(TObject)
  private
    FLog: TEventLog; //Logging/debug output to syslog/eventlog
  public
    property EventLog: TEventLog read FLog;
    // Write to log and optionally console with seriousness etInfo
    procedure WriteLog(Message: string; ToConsole: boolean = False);
    // Write to log and optionally console with specified seriousness
    procedure WriteLog(EventType: TEventType; Message: string;
      ToConsole: boolean = False);
    constructor Create;
    destructor Destroy; override;
  end;

var
  TigerLog: TLogger;
//Created by unit initialization so available for every referencing unit

//Shows non-debug messages on screen; also shows debug messages if DEBUG defined
procedure infoln(Message: string; Level: TEventType);

implementation

procedure infoln(Message: string; Level: TEventType);
var
  Seriousness: string;
begin
  case Level of
    etCustom: Seriousness := 'Custom:';
    etDebug: Seriousness := 'Debug:';
    etInfo: Seriousness := 'Info:';
    etWarning: Seriousness := 'WARNING:';
    etError: Seriousness := 'ERROR:';
    else
      Seriousness := 'UNKNOWN CATEGORY!!:'
  end;
  if (Level <> etDebug) then
  begin
    if AnsiPos(LineEnding, Message) > 0 then
      writeln(''); //Write an empty line before multiline messagse
    writeln(Seriousness + ' ' + Message); //we misuse this for info output
    sleep(200); //hopefully allow output to be written without interfering with other output
  end
  else
  begin
      {$IFDEF DEBUG}
      {DEBUG conditional symbol is defined using e.g.
      Project Options/Other/Custom Options using -dDEBUG}
    if AnsiPos(LineEnding, Message) > 0 then
      writeln(''); //Write an empty line before multiline messagse
    writeln(Seriousness + ' ' + Message); //we misuse this for info output
    sleep(200); //hopefully allow output to be written without interfering with other output
      {$ENDIF DEBUG}
  end;
end;

{ TLogger }

procedure TLogger.WriteLog(Message: string; ToConsole: boolean = False);
begin
  FLog.Log(etInfo, Message);
  if ToConsole then
    infoln(Message, etinfo);
end;

procedure TLogger.WriteLog(EventType: TEventType; Message: string;
  ToConsole: boolean = False);
begin
  // Only log debug level if compiled as a debug build in order to cut down on logging
  {$IFDEF DEBUG}
  if 1 = 1 then
  {$ELSE}
    if EventType <> etDebug then
  {$ENDIF}
    begin
      FLog.Log(EventType, Message);
      if ToConsole then
        infoln(Message, etinfo);
    end;
  {$IFDEF DEBUG}
  // By setting active to false, we try to force a log write. Next log attempt will set active to true again
  FLog.Active := False;
  {$ENDIF}
end;

constructor TLogger.Create;
begin
  FLog := TEventLog.Create(nil);
  FLog.LogType := ltSystem; //eventlog/syslog, not log to file
  FLog.RegisterMessageFile('');
  //specify Windows should use the binary to look up formatting strings
  FLog.RaiseExceptionOnError := False; //Don't throw exceptions on log errors.
  FLog.Active := True;
end;

destructor TLogger.Destroy;
begin
  FLog.Active := False; //save WriteLog text
  FLog.Free;
  inherited Destroy;
end;

initialization
  begin
    TigerLog := TLogger.Create;
  end;

finalization
  begin
    TigerLog.Free;
  end;
end.
