with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Debug_Put;
pragma Elaborate_All (Crypto.Symmetric.KDF);


package Crypto.Symmetric.KDF_Scrypt is

   type W_Block512_Array is array (Integer range <>) of W_Block512;

   package Error_Output is new Crypto.Debug_Put(b => false);

   array_size_not_equal_exception : exception ;
   N_not_power_of_2_exception : exception;

   package KDF is new Crypto.Symmetric.KDF(Return_Type        => W_Block512,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);
   use KDF;

   type Scrypt_KDF is new KDF.KDF_Scheme with private;


   --Interface function for static 64 Bytes Output, assuming p=8, r=8 and N=Security_Parameter
   overriding
   procedure Derive(This	: in out Scrypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	W_Block512);

   --function for setting security parameter, used here for setting round count
   overriding
   procedure Initialize(This	: out Scrypt_KDF;
                       Key_Length: in Natural);


   procedure Initialize (This	: out Scrypt_KDF;
                         r		: in 	Natural;
                         N		: in 	Natural;
                         p		: in	Natural;
                         dkLen	: in	Natural);

   --core scrypt function
   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes);

   --Block rearrangement, used by Scrypt_Block_Mix
   function Scrypt_ROMix(Input	: in 	W_Block512_Array;
                         N	: in 	Natural) return W_Block512_Array;

   --Function Scrypt_Block_Mix, used by scrypt
   function Scrypt_Block_Mix(Input	: in W_Block512_Array) return W_Block512_Array;

   --Stream Cipher, used by Scrypt_Block_Mix
   procedure Salsa20_8(Input	: in	W_Block512;
                       Output 	: out	W_Block512);

   --XORing function for type W_Block512_Array
   function "xor" (Left : W_Block512_Array;
                   Right: W_Block512_Array) return W_Block512_Array;

   --power of two test (rudimentary)
   function IsPowerOfTwo(value : Natural) return Boolean;

   private
   type Scrypt_KDF is new KDF.KDF_Scheme with
      record
         r 			: Natural :=8;
         N 			: Natural :=8;
         p 			: Natural :=8;
         dkLen 			: Natural;

      end record;


end Crypto.Symmetric.KDF_Scrypt;
