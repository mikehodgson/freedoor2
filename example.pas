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

uses FreeDoor;

var
  Door: TDoor;

begin
  try
    Door := TDoor.Create(true);
  finally
    Door.Free;
  end;
end.
