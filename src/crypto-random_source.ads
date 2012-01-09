with Ada.Finalization;
with Crypto.Types;

package Crypto.Random_Source is
  use Crypto.Types;
  package Fin renames Ada.Finalization;
    
  type Random_Source is abstract new Fin.Controlled with null record;
  
  Random_Source_Read_Error : exception;
  
  procedure Initialize (This: in out Random_Source) is abstract;
  
  procedure Read(This : in Random_Source; B : out Byte)  is abstract;
  
  procedure Read(This : in Random_Source'Class; Byte_Array : out Bytes);
  
  procedure Read(This : in Random_Source; B : out B_Block128) is null;-- is abstract;
  
  procedure Read(This : in Random_Source; W : out Word) is null;--  is abstract;
  procedure Read(This : in Random_Source; Word_Array : out Words) is null;-- is abstract;

  procedure Read(This : in Random_Source; D : out DWord) is null;--  is abstract;
  procedure Read(This : in Random_Source; DWord_Array : out DWords) is null;-- is abstract;
end Crypto.Random_Source;
