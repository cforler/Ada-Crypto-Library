with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Hashfunction_SHA512;
pragma Elaborate_All (Crypto.Symmetric.KDF);


package Crypto.Symmetric.KDF_Scrypt is

   type W_Block512_Array is array (Integer range <>) of W_Block512;

   array_size_not_equal_exception : exception ;



--     package KDF is new Crypto.Symmetric.KDF(return_type        => W_Block512,
--                                             security_parameter => Natural,
--                                             H                  => Crypto.Symmetric.Hashfunction_SHA512);
--     use KDF;

--     type Scrypt_KDF is new KDF.KDF_Scheme with private;



--     overriding
--     procedure Derive(This	: in out PBKDF2_KDF;
--                      Salt	: in 	String;
--                      Password	: in	String;
--                      Key		: out	W_Block512);
--
--
--     overriding
--     function Initialize(This	: out PBKDF2_KDF;
--                         Parameter: in Natural) return Boolean;

   function Scrypt_Block_Mix(Input	: in W_Block512_Array) return W_Block512_Array;


   function Scrypt_ROMix(Input	: in 	W_Block512_Array;
                          N	: in 	Natural) return W_Block512_Array;




   procedure Salsa20_8(Input	: in	W_Block512;
                       Output 	: out	W_Block512);

   function R(A : Word;
              B : Natural) return Word;

   function "xor" (Left : W_Block512_Array;
                   Right: W_Block512_Array) return W_Block512_Array;

   function Reverse_Bits(Input : Byte) return Byte;

   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes);






private

--     type Scrypt_KDF is new KDF.KDF_Scheme with
--        record
--           Security_Parameter	: Natural;
--        end record;


end Crypto.Symmetric.KDF_Scrypt;
