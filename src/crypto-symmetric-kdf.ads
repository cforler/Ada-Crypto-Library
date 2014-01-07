with Crypto.Types;
with Crypto.Symmetric.Hashfunction;

generic
   type return_type is private;
   type security_parameter is private;

   with package H is new Crypto.Symmetric.Hashfunction(<>);

package Crypto.Symmetric.KDF is

   type KDF_Scheme is limited interface;

   procedure Derive(This	: in out KDF_Scheme;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	return_type) is abstract;

   function Initialize(This	: out KDF_Scheme;
                       Parameter: in security_parameter) return Boolean is abstract;

   Invalid_Key_Type_Exception	: exception;



private


end Crypto.Symmetric.KDF;
