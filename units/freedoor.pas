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
  TFDObj = class
  private
    FDebug:     Boolean;
  public
    procedure   DebugWriteLn(str: string);
    function    GetDebug(): Boolean;
    procedure   SetDebug(Dbg: Boolean);
  end;

type
  TUser = class(TFDObj)
  private
    FName:      string;
    FHandle:    string;
    FLocation:  string;
    FBaudRate:  Integer;
    FNode:      Integer;
    FPort:      Integer;
    FLevel:     Integer;
    FTime:      Integer;
    FAnsi:      Boolean;
    FRemote:    Boolean;
  public
    constructor Create(dfn: string; Dbg: Boolean = false);
    destructor  Destroy; override;
  end;

type
  TDoor = class(TFDObj)
  private
    FDropFile:  string;
    FUser:      TUser;
    procedure   ParseCommandLine;
    procedure   SetDropFile(dfn: string);
  public
    constructor Create(Dbg: Boolean = false);
    destructor  Destroy; override;
    function    GetDropFile(): string;
  end;

implementation

  { TFDObj Class }
  
  procedure TFDObj.DebugWriteLn(Str: string);
  begin
    if (FDebug = true) then
      WriteLn('### ' + Str);
  end;
  
  function TFDObj.GetDebug(): Boolean;
  begin
    GetDebug := FDebug;
  end;
  
  procedure TFDObj.SetDebug(Dbg: Boolean);
  begin
    FDebug := Dbg;
  end;

  { TUser Class }
  
  constructor TUser.Create(dfn: string; Dbg: Boolean = false);
  begin
    SetDebug(Dbg);
    DebugWriteLn('User object created');
  end;
  
  destructor TUser.Destroy;
  begin
    DebugWriteLn('User object destroyed');
    inherited;
  end;

  { TDoor Class }

  constructor TDoor.Create(Dbg: boolean = false);
  begin
    SetDebug(true);
    DebugWriteLn('Door object created');
    ParseCommandLine;
    FUser := TUser.Create(GetDropFile(), getDebug());
  end;
    
  destructor TDoor.Destroy;
  begin
    FreeAndNil(FUser);
    DebugWriteLn('Door object destroyed');
    inherited;
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
