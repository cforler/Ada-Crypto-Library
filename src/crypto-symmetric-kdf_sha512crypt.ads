with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction_SHA512;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_SHA512Crypt is

   subtype S5C_String is String(1..86) ;

   package KDF is new Crypto.Symmetric.KDF(return_type        => S5C_String,
                                           security_parameter => Natural,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);

   type SHA512Crypt_KDF is new KDF.KDF_Scheme with private;

   overriding
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	S5C_String);



   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface);

   overriding
   function Initialize(This	: out SHA512Crypt_KDF;
                       Parameter: in Natural) return Boolean;

private

   type SHA512Crypt_KDF is new KDF.KDF_Scheme with
      record
         Security_Parameter	: Natural;
      end record;

   function To_Binary(N: Natural) return String;

   function To_Natural(S : String) return Natural;

   function To_Base64(N : Natural) return Character;

   function Bytes_To_String(B : bytes) return String;


end Crypto.Symmetric.KDF_SHA512Crypt;
