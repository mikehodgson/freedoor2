(* **************************************************************************
 * name:       FreeDoor BBS Door Engine
 * version:    2.0.0
 * author:     Mike Hodgson
 * copyright:  2014, Mike Hodgson
 * licence:    The MIT License
 *             http://opensource.org/licenses/MIT
 * credits:    Mike Hodgson, Michael Preslar, Rick Parrish, Maarten Bekers,
 *             Sean Dennis
 * **************************************************************************)

unit FreeDoor;

{$mode objfpc}

interface

uses  Classes, StrUtils, SysUtils,{$IFDEF WIN32} Windows, {$ENDIF}Crt, Dos, Ansi,
      EleNorm;

type
  TFDObj = class
  private
    FDebug:     Boolean;
    procedure   DbgWriteLn(Text: string);
  public
    property    Debug: Boolean read FDebug write FDebug;
  end;

type
  TUser = class(TFDObj)
  private
    FName:      string;
    FHandle:    string;
    FLocation:  string;
    FBBS:       string;
    FBaudRate:  Integer;
    FConnType:  Integer;
    FNode:      Integer;
    FPort:      Integer;
    FLevel:     Integer;
    FTime:      Integer;
    FAnsi:      Boolean;
    FRemote:    Boolean;
  public
    constructor Create(Dbg: Boolean = false);
    destructor  Destroy; override;
    property    Name: string read FName write FName;
    property    Handle: string read FHandle write FHandle;
    property    Location: string read FLocation write FLocation;
    property    BBS: string read FBBS write FBBS;
    property    BaudRate: Integer read FBaudRate write FBaudRate;
    property    ConnType: Integer read FConnType write FConnType;
    property    Node: Integer read FNode write FNode;
    property    Port: Integer read FPort write FPort;
    property    Level: Integer read FLevel write FLevel;
    property    Time: Integer read FTime write FTime;
    property    ANSI: Boolean read FAnsi write FAnsi;
    property    Remote: Boolean read FRemote write FRemote;
  end;

type
  TDoor = class(TFDObj)
  private
    FDropFile:  string;
    FLocalOnly: Boolean;
    FUser:      TUser;
    procedure   ParseCommandLine;
    procedure   GetLocalInformation;
    procedure   SetDropFile(DropFileName: string);
  public
    constructor Create(Dbg: Boolean = false);
    destructor  Destroy; override;
    procedure   ErrorWriteLn(Err: string);
    property    DropFile: string read FDropFile write SetDropFile;
    property    LocalOnly: Boolean read FLocalOnly write FLocalOnly;
    property    User: TUser read FUser;
  end;

implementation

  (* TFDObj Base Class *)
  
  procedure TFDObj.DbgWriteLn(Text: string);
  begin
    if (FDebug) then
      WriteLn('### ' + Text);
  end;
  
  (* TUser Class *)
  
  constructor TUser.Create(Dbg: Boolean = false);
  begin
    FDebug := Dbg;
    DbgWriteLn('User object created');
  end;
  
  destructor TUser.Destroy;
  begin
    DbgWriteLn('User object destroyed');
    inherited;
  end;
  

  (* TDoor Class *)

  constructor TDoor.Create(Dbg: Boolean = false);
  begin
    FLocalOnly := true;
    FDebug := Dbg;
    DbgWriteLn('Door object created');
    FUser := TUser.Create(FDebug);
    ParseCommandLine;
  end;
    
  destructor TDoor.Destroy;
  begin
    FreeAndNil(FUser);
    DbgWriteLn('Door object destroyed');
    inherited;
  end;
  
  procedure TDoor.ErrorWriteLn(Err: string);
  begin
    TextColor(4);
    WriteLn(Err);
    TextColor(7);
  end;
    
  procedure TDoor.ParseCommandLine;
  var
    I: Integer;
  begin
    for I := 1 to ParamCount do
    begin
       case UpperCase(ParamStr(I)) of
       
        (* Read drop file *)
        '/D': begin
          SetDropFile(ParamStr(I + 1));
          DbgWriteLn('Dropfile set to: ' + FDropFile);
        end;
        
        (* Local only, ask user for information *)
        '/L': begin
          FLocalOnly := true;
          DbgWriteLn('Entering local only mode...');
          GetLocalInformation;
        end;
      end;
    end;
  end;
  
  procedure TDoor.GetLocalInformation;
  begin
  end;
  
  procedure TDoor.SetDropFile(DropFileName: string);
    var directory: DirStr;
    var name: NameStr;
    var ext: ExtStr;
    
    procedure ReadDorinfoDef;
    begin
    end;
    
    procedure ReadDoorSys;
    begin
    end;
    
    procedure ReadDoor32Sys;
      var f: text;
      var s: string;
      var i: integer;
    begin
      assign(f, FDropFile);
      reset(f);
      readln (f,s);
      val(s,FUser.ConnType,i);
      readln (f,s);
      val(s,FUser.Port,i);
      readln (f,s);
      val(s,FUser.BaudRate,i);
      readln (f,s);
      FUser.BBS := s;
      readln (f,s);
      readln (f,s);
      FUser.Name := s;
      readln (f,s);
      FUser.Handle := s;
      readln (f,s);
      val(s,FUser.Level,i);
      readln (f,s);
      val(s,FUser.Time,i);
      readln (f,s);
      if (s = '1') then FUser.ANSI := true else FUser.ANSI := false;
      readln (f,s);
      val(s,FUser.Node,i);
      close(f);
      DbgWriteLn('Door32.sys read succesfully!');
    end;
    
    procedure ReadXtrnDat;
    begin
    end;
    
  begin
    FDropFile := DropFileName;
    FSplit(AnsiLowerCase(FDropFile), directory, name, ext);
    if not (FileExists(FDropFile)) then
    begin
      raise Exception.Create('Drop file does not exist: ' + FDropFile);
    end;
    if (name = 'door') then
      ReadDoorSys
    else if (name = 'door32') then
      ReadDoor32Sys
    else if (pos('dorinfo', name) > 0) then
      ReadDorinfoDef
    else if (name = 'xtrn') then
      ReadXtrnDat
    else
      raise Exception.Create('Invalid drop file format. Valid formats: door.sys, door32.sys, dorinfox.def, xtrn.dat');
  end;
  
end.
