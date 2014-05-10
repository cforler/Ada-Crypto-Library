with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Hashfunction_SHA1;
with Crypto.Symmetric.Mac.Hmac_SHA1;
with Crypto.Debug_Put;

pragma Elaborate_All (Crypto.Symmetric.KDF);
with Crypto.Symmetric.Mac.Hmac;


generic
   with package Hmac_Package is new Crypto.Symmetric.Mac.Hmac(<>);
   with function To_Message_Type(B : Bytes) return Hmac_Package.H.Message_Type;
   with function To_Bytes(M : Hmac_Package.H.Hash_Type) return Bytes;
   with function "xor"(Left : Hmac_Package.H.Hash_Type;
                       Right: Hmac_Package.H.Hash_Type) return Hmac_Package.H.Hash_Type;
package Crypto.Symmetric.KDF_PBKDF2 is

   package KDF is new Crypto.Symmetric.KDF(Return_Type        => Bytes,
                                           H                  => Hmac_Package.H);
   use KDF;

   type PBKDF2_KDF is new KDF.KDF_Scheme with private;


   --Interface function for static 64 Bytes Output
   overriding
   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes);

   --actual derivation function, pure PBKDF2
   procedure PBKDF2(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
      		    DK_Len	: in 	Natural);

   --function for setting Key Length
   overriding
   procedure Initialize(This	: out PBKDF2_KDF;
                        Key_Length: in Natural);

   --function for setting Key Length and security parameter, used here for setting round count in F_Function
   procedure Initialize(This		: out PBKDF2_KDF;
                        Key_Length	: in Natural;
                        Round_Count	: in Natural);

private

   package Error_Output is new Crypto.Debug_Put(b => true);

   --Internal function for applying PRF multiple times
   function F_Function(Salt	: in 	Bytes;
                       Password	: in	Bytes;
                       Count	: in 	Natural;
                       Round	: in 	Natural) return Hmac_Package.H.Hash_Type;

   type PBKDF2_KDF is new KDF.KDF_Scheme with
      record
         Round_Count		: Natural := 4096;
         Key_Length		: Natural := 64;
      end record;

end Crypto.Symmetric.KDF_PBKDF2;
