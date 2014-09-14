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
unit freedoor;

{$mode objfpc}

interface

uses  classes, sysutils,{$IFDEF WIN32} windows, {$ENDIF}crt, dos, ansi,
      elenorm;

type
  TUser = class
  private
    name:       string;
    handle:     string;
    location:   string;
    baudRate:   integer;
    node:       integer;
    port:       integer;
    level:      integer;
    time:       integer;
    hasANSI:    boolean;
    isRemote:   boolean;

    procedure debugWriteLn(str: string);
  public
    constructor Create(dfn: string);
    destructor  Destroy; override;
  end;

type
  TDoor = class
  private
    dropFile:   string;
    user:       TUser;
    debug:      boolean;
    
    procedure parseCommandLine;
    procedure debugWriteLn(str: string);
    procedure setDropFile(dfn: string);
  public
    constructor Create;
    destructor Destroy; override;
    function getDropFile(): string;
  end;

implementation

  { TUser Object }
  
  constructor TUser.Create(dfn: string);
  begin
    debugWriteLn('User object created');
  end;
  
  destructor TUser.Destroy;
  begin
    debugWriteLn('User object destroyed');
    inherited;
  end;

  procedure TUser.debugWriteLn(str: string);
  begin
    writeLn('### ' + str);
  end;

  { TDoor Object }

  constructor TDoor.Create;
  begin
    debug := true;
    debugWriteLn('Door object created');
    parseCommandLine;
    user := TUser.Create(getDropFile());
  end;
    
  destructor TDoor.Destroy;
  begin
    freeAndNil(user);
    debugWriteLn('Door object destroyed');
    inherited;
  end;
    
  procedure TDoor.debugWriteLn(str: string);
  begin
    if debug = true then
      writeLn('>>> ' + str);
  end;
  
  procedure TDoor.parseCommandLine;
  var
    i: integer;
  begin
    for i := 1 to paramCount do
    begin
       case upperCase(paramStr(i)) of
       
        { Read drop file }
        '/D': begin
          setDropFile(paramStr(i + 1));
          debugWriteLn('Dropfile set to: ' + getDropFile());
        end;
        
        { Local only, ask user for information }
        '/L': begin
          debugWriteLn('Entering local only mode...');
        end;
      end;
    end;
  end;
  
  procedure TDoor.setDropFile(dfn: string);
  begin
    dropFile := dfn;
  end;
  
  function TDoor.getDropFile(): string;
  begin
    getDropFile := dropFile;
  end;
end.
