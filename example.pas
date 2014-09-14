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

uses freedoor;

var
  door: TDoor;

begin
  try
    door := TDoor.Create;
  finally
    door.Free;
  end;
end.
