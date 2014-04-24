with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Debug_Put;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_SHA512Crypt is

   subtype S5C_String is String(1..86) ;

   package KDF is new Crypto.Symmetric.KDF(return_type        => S5C_String,
                                           security_parameter => Natural,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);

   type SHA512Crypt_KDF is new KDF.KDF_Scheme with private;

   package Error_Output is new Crypto.Debug_Put(b => false);

   --Interface and core function
   overriding
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	S5C_String);

   --Interface and core function
   overriding
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	S5C_String);


   --Initializing Security_Parameter, used for round count
   overriding
   function Initialize(This	: out SHA512Crypt_KDF;
                       Parameter: in Natural) return Boolean;





   --Adding Bytes to Digest
   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface);
private
   type SHA512Crypt_KDF is new KDF.KDF_Scheme with
      record
         Security_Parameter	: Natural;
      end record;

   --translates 3 bytes into base64 4 char string
   function Bytes_To_String(B : bytes) return String;

   --returns a string of 1 and 0 from a number
   function To_Binary(N: Natural) return String;

   --returns a number from a string of 1 and 0
   function To_Natural(S : String) return Natural;

   --translates number into base64 string
   function To_Base64(N : Natural) return Character;




end Crypto.Symmetric.KDF_SHA512Crypt;
