with Ada.Finalization;

package Crypto.Types.Random_Source is
  use Crypto.Types;
  package Fin renames Ada.Finalization;
    
  type Random_Source is abstract new Fin.Controlled with null record;
  type Random_Source_Access is access Random_Source;
  
  Random_Source_Read_Error : exception;
  
  procedure Initialize (This: in out Random_Source) is abstract;
  
  procedure Read(This : in Random_Source; B : out Byte)  is abstract;
  
  procedure Read(This : in Random_Source; Byte_Array : out Bytes);
  
  procedure Read(This : in Random_Source; B : out B_Block128);
  
  procedure Read(This : in Random_Source; W : out Word);
  procedure Read(This : in Random_Source; Word_Array : out Words);

  procedure Read(This : in Random_Source; D : out DWord);
  procedure Read(This : in Random_Source; DWord_Array : out DWords);
  
  pragma Inline(Read);
end Crypto.Types.Random_Source;
