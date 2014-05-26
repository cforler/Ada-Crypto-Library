with Ada.Finalization;


generic
   type Base64_Character is (<>);
package Crypto.Types.Base64 is

   type Base64_String  is array (Positive range <>) of Base64_Character;
   subtype Base64_SHA512Crypt is Base64_String(1..86);

   function Encode_Base64(B: Bytes) return Base64_String;


end Crypto.Types.Base64;
