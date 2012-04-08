--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--with Ada.Text_IO; use Ada.Text_IO;

package body  Crypto.Symmetric.Mode is
  
   ---------------------------------------------------------------------------
   
   function To_Block(Input : in Bytes; OutLen : in Positive) return Bytes is
   begin
      if Input'Length = OutLen then
	return Input;
      else 
        declare
	   Result : Bytes(0..OutLen-1) := (others => 0);
	begin 
	   Result(Input'Range) := Input;
	   return Result;
	end;
      end if;
   end To_Block;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(Block : Bytes) return Bytes is
   begin
      return Block;
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   procedure Set_Zero(Block : out Bytes) is
   begin
      Block := (Others => 0);
   end Set_Zero;
   
end  Crypto.Symmetric.Mode;
   
