package body Crypto.Symmetric.Utils is

   function Password_To_Key(Password : in String) return Key_Type is
      B: constant Bytes := Hashfunction.To_Bytes(Hashfunction.Hash(Password));
      Bytes_Per_Key_Block: constant Positive := Key_Type'Size / 8;
   begin
      if B'Length < Bytes_Per_Key_Block then
         raise Invalid_Key_Length;
      else
         return To_Key(B(B'First..B'First+Bytes_Per_Key_Block-1));
      end if;
   end Password_To_Key;

end Crypto.Symmetric.Utils;