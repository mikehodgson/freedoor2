{
* **************************************************************************
* name:       FreeDoor BBS Door Engine
* version:    2.0.0
* author:     Mike Hodgson
* copyright:  2014, Mike Hodgson
* licence:    The MIT License
*             http://opensource.org/licenses/MIT
* credits:    Mike Hodgson, Michael Preslar, Rick Parrish, Maarten Bekers,
*             Sean Dennis
* **************************************************************************
}
unit FreeDoor;

{$mode objfpc}

interface

uses  Classes, SysUtils,{$IFDEF WIN32} Windows, {$ENDIF}Crt, Dos, Ansi,
      EleNorm;

type
  TUser = class
  private
    FName:      string;
    FHandle:    string;
    FLocation:  string;
    FBaudRate:  Integer;
    FNode:      Integer;
    FPort:      Integer;
    FLevel:     Integer;
    FTime:      Integer;
    FAnsi:      boolean;
    FRemote:    boolean;
    procedure   DebugWriteLn(str: string);
  public
    constructor Create(dfn: string);
    destructor  Destroy; override;
  end;

type
  TDoor = class
  private
    FDropFile:  string;
    FUser:      TUser;
    FDebug:     boolean;
    procedure   ParseCommandLine;
    procedure   DebugWriteLn(str: string);
    procedure   SetDropFile(dfn: string);
  public
    constructor Create;
    destructor  Destroy; override;
    function    GetDropFile(): string;
  end;

implementation

  { TUser Object }
  
  constructor TUser.Create(dfn: string);
  begin
    DebugWriteLn('User object created');
  end;
  
  destructor TUser.Destroy;
  begin
    DebugWriteLn('User object destroyed');
    inherited;
  end;

  procedure TUser.DebugWriteLn(Str: string);
  begin
    WriteLn('### ' + Str);
  end;

  { TDoor Object }

  constructor TDoor.Create;
  begin
    FDebug := true;
    DebugWriteLn('Door object created');
    ParseCommandLine;
    FUser := TUser.Create(GetDropFile());
  end;
    
  destructor TDoor.Destroy;
  begin
    FreeAndNil(FUser);
    DebugWriteLn('Door object destroyed');
    inherited;
  end;
    
  procedure TDoor.DebugWriteLn(str: string);
  begin
    if FDebug = true then
      WriteLn('>>> ' + str);
  end;
  
  procedure TDoor.ParseCommandLine;
  var
    I: Integer;
  begin
    for I := 1 to ParamCount do
    begin
       case UpperCase(ParamStr(I)) of
       
        { Read drop file }
        '/D': begin
          SetDropFile(ParamStr(I + 1));
          DebugWriteLn('Dropfile set to: ' + GetDropFile());
        end;
        
        { Local only, ask user for information }
        '/L': begin
          DebugWriteLn('Entering local only mode...');
        end;
      end;
    end;
  end;
  
  procedure TDoor.setDropFile(Dfn: string);
  begin
    FDropFile := Dfn;
  end;
  
  function TDoor.getDropFile(): string;
  begin
    GetDropFile := FDropFile;
  end;
end.
