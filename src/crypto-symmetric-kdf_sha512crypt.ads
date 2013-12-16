with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction_SHA512;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_SHA512Crypt is



   type salt_type is array(0..9) of Character;

   package KDF is new Crypto.Symmetric.KDF(return_type        => W_Block512,
                                           security_parameter => Natural,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);

   procedure Derive(Salt	: in 	salt_type;
                    Password	: in	String;
                    Key		: out	W_Block512);

   procedure Derive(Salt	: in 	String;
                    Password	: in	String);

   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	DW_Block512);

   function Initialize(Parameter	: in	Natural) return Boolean;

   function To_Binary(N: Natural) return String;

   function To_Natural(S : String) return Natural;

   function To_Base64(N : Natural) return Character;

   function Bytes_To_String(B : bytes) return String;


end Crypto.Symmetric.KDF_SHA512Crypt;
