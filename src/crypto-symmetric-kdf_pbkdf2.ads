with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Hashfunction_SHA1;
with Crypto.Symmetric.Mac.Hmac_SHA1;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_PBKDF2 is

   type salt_type is array(0..9) of Character;

   package KDF is new Crypto.Symmetric.KDF(return_type        => W_Block512,
                                           security_parameter => Natural,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA1);
   use KDF;

   package Hmac_Package renames Crypto.Symmetric.Mac.Hmac_SHA1;

   procedure Derive(Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	W_Block512);

   procedure Derive(Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural);

   procedure Derive(Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
      		    DK_Len	: in 	Natural);

   function Initialize(Parameter	: in	Natural) return Boolean;

private
   Rounds : Natural;
   function F_Function(Salt	: in 	Bytes;
                       Password	: in	Bytes;
                       Count	: in 	Natural;
                       Round	: in 	Natural) return W_Block160;

end Crypto.Symmetric.KDF_PBKDF2;
