{
* **************************************************************************
* name:       FreeDoor BBS Door Engine - Example Door
* version:    0.0.1
* author:     Mike Hodgson
* copyright:  2014, Mike Hodgson
* licence:    The MIT License
*             http://opensource.org/licenses/MIT
* **************************************************************************
}

uses FreeDoor, SysUtils;

var
  Door: TDoor;
  inp:  string;
  ch:   string;
begin
  inp := '';
  ch := '';
  try
    try
      Door := TDoor.Create(true);
      {
      Door.WriteLn('The WriteLn command supports ANSI, |17|15pipe|16|07, and `5FTelegard`07 colour codes.');
      Door.WriteLn('Please enter some text below:');
      Door.ReadLn(inp);
      Door.WriteLn('You wrote: ', inp);
      Door.WriteLn('Thanks for playing, ', Door.User.Name, '. Press a key to exit.');
      Door.Read(ch);
      }
    except
      on E: Exception do Door.ErrorWriteLn(E.Message);
    end;
  finally
    Door.Free;
  end;
end.
