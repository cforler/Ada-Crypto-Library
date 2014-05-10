with Crypto.Types;
use Crypto.Types;
with Crypto.Symmetric.Hashfunction;

package body Crypto.Symmetric.KDF is


   procedure Derive(This	: in out KDF_Scheme'Class;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	return_type) is
   begin

      This.Derive(Salt     => To_Bytes(Message => Salt),
                  Password => To_Bytes(Message => Password),
                  Key      => Key);

   end;



end Crypto.Symmetric.KDF;
