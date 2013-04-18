with Crypto.Types.Nonces;
with Crypto.Types;

generic
   type Key_Type is private;
   type Block is private;

   with package N  is new Crypto.Types.Nonces(Block);

package Crypto.Symmetric.AE is
   use Crypto.Types;

   type AE_Scheme is limited interface;
   type Callback_Writer is access procedure (B : in  Bytes);
--     type Callback_Reader is access function (B: out Bytes) return Natural;
   type Callback_Reader is access procedure (B : out Bytes; Count: out Natural);

   procedure Init_Encrypt(This   : out    AE_Scheme;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class) is abstract;

   procedure Init_Decrypt(This        : out AE_Scheme;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block) is abstract;

   procedure Encrypt(This             : in out AE_Scheme;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is abstract;

   function Decrypt_And_Verify(This                   : in out AE_Scheme;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is abstract;

  -- Invalid_Ciphertext_Error   : exception;
   Invalid_Header_Error       : exception;
   Invalid_Key_Length         : exception;

end Crypto.Symmetric.AE;
