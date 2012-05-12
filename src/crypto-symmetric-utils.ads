with Crypto.Types;
with Crypto.Symmetric.Hashfunction;

use Crypto.Types;

generic
   with package Hashfunction is new Crypto.Symmetric.Hashfunction(<>);
   type Key_Type is private;
   with function To_Key(B : in Bytes) return Key_Type is <>;

package Crypto.Symmetric.Utils is

   function Password_To_Key(Password : in String) return Key_Type;

private
   Invalid_Key_Length: exception;
end Crypto.Symmetric.Utils;