with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Hashfunction_SHA1;
with Crypto.Symmetric.Mac.Hmac_SHA1;

pragma Elaborate_All (Crypto.Symmetric.KDF);
with Crypto.Symmetric.Mac.Hmac;


generic
   with package Hmac_Package is new Crypto.Symmetric.Mac.Hmac(<>);
   with function To_Message_Type(B : Bytes) return Hmac_Package.H.Message_Type;
   with function To_Bytes(M : Hmac_Package.H.Hash_Type) return Bytes;
   with function "xor"(Left : Hmac_Package.H.Hash_Type;
                       Right: Hmac_Package.H.Hash_Type) return Hmac_Package.H.Hash_Type;
package Crypto.Symmetric.KDF_PBKDF2 is


   package KDF is new Crypto.Symmetric.KDF(return_type        => W_Block512,
                                           security_parameter => Natural,
                                           H                  => Hmac_Package.H);
   use KDF;

   --package Hmac_Package renames Crypto.Symmetric.Mac.Hmac_SHA1;

   type PBKDF2_KDF is new KDF.KDF_Scheme with private;



   overriding
   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	W_Block512);

   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural);

   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
      		    DK_Len	: in 	Natural);

   overriding
   function Initialize(This	: out PBKDF2_KDF;
                       Parameter: in Natural) return Boolean;

private

   type PBKDF2_KDF is new KDF.KDF_Scheme with
      record
         Security_Parameter	: Natural;
      end record;

   function F_Function(Salt	: in 	Bytes;
                       Password	: in	Bytes;
                       Count	: in 	Natural;
                       Round	: in 	Natural) return Hmac_Package.H.Hash_Type;

end Crypto.Symmetric.KDF_PBKDF2;
