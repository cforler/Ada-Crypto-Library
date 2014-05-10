with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Debug_Put;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_SHA512Crypt is

   package KDF is new Crypto.Symmetric.KDF(Return_Type        => Base64_String,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);

   type SHA512Crypt_KDF is new KDF.KDF_Scheme with private;




   --Interface and core function
   overriding
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Base64_String);


   --function for setting Key Length
   overriding
   procedure Initialize(This	: out SHA512Crypt_KDF;
                        Key_Length: in Natural);

   --function for setting Key Length and security parameter, used here for setting round count in F_Function
   procedure Initialize(This		: out SHA512Crypt_KDF;
                        Key_Length	: in Natural;
                        Round_Count	: in Natural);


   --Adding Bytes to Digest
   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	Crypto.Symmetric.Algorithm.SHA512.Sha512_Context);
private

   package Error_Output is new Crypto.Debug_Put(b => false);
   type SHA512Crypt_KDF is new KDF.KDF_Scheme with
      record
         Key_Length	: Natural;
         Round_Count	: Natural :=5000;
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
