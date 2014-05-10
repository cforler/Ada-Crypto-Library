with Crypto.Types;
use Crypto.Types;
with Crypto.Symmetric.Hashfunction;

generic
   type Return_Type(<>) is private;

   with package H is new Crypto.Symmetric.Hashfunction(<>);

package Crypto.Symmetric.KDF is

   type KDF_Scheme is abstract tagged null record;

   procedure Derive(This	: in out KDF_Scheme'Class;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Return_Type);

   procedure Derive(This	: in out KDF_Scheme;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Return_Type) is abstract;

   procedure Initialize(This	: out KDF_Scheme;
                        Key_Length : in Natural) is abstract;




private


end Crypto.Symmetric.KDF;
