with Crypto.Types;
with Crypto.Symmetric.Hashfunction;

generic
   type return_type is private;
   type security_parameter is private;

   with package H is new Crypto.Symmetric.Hashfunction(<>);

package Crypto.Symmetric.KDF is

     procedure Derive(Salt	: in 	String;
                      Password	: in	String;
                      Key	: out	return_type) is abstract;

     function Initialize(Parameter	: in	security_parameter) return Boolean is abstract;

     type KDF_Scheme is limited interface;

     Invalid_Key_Type         : exception;

private


end Crypto.Symmetric.KDF;
