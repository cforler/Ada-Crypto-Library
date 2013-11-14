with Crypto.Types;
with Crypto.Symmetric.Hashfunction;

generic
   type Key_Type is private;

   with package H is new Crypto.Symmetric.Hashfunction(<>);

package Crypto.Symmetric.KDF is

   procedure Derive(Password	: in	String;
                    Key		: out	Key_Type) is abstract;

   type KDF_Scheme is limited interface;

   Invalid_Key_Type         : exception;

end Crypto.Symmetric.KDF;
