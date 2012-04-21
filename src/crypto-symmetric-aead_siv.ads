with Crypto.Symmetric.AE;
with Crypto.Symmetric.AE.AD;
with Crypto.Symmetric.Blockcipher;
with Crypto.Types.Nonces;
with Crypto.Types; use Crypto.Types;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>);
   with package N is new Crypto.Types.Nonces(BC.Block);

   with function "xor" (Left, Right : in BC.Block) return BC.Block is <>;        -- CMAC,CTR
   with function To_Block_Type (B : Bytes) return BC.Block;                      -- CMAC
   with function To_Bytes (B : BC.Block) return Bytes;                           -- CMAC
   with function Shift_Left (Value: BC.Block; Amount: Natural) return BC.Block;  -- CMAC
   with function "+" (Left: BC.Block; Right : in Byte) return BC.Block is <>;    -- CTR

package Crypto.Symmetric.AEAD_SIV is
   use BC;

   package AE is new Crypto.Symmetric.AE(Key_Type => Key_Type,
                                         Block    => Block,
                                         N        => N);
   use AE;

   package AEAD is new AE.AD;

   type AEAD_SIV is new AE.AE_Scheme and AEAD.AEAD_Scheme with private;

   ---------------------------------------------
   ---- overriding functions and procedures ----
   ---------------------------------------------

   overriding
   procedure Init_Encrypt(This   : out    AEAD_SIV;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class);

   overriding
   procedure Init_Decrypt(This        : out AEAD_SIV;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block);

   overriding
   procedure Encrypt(This             : in out AEAD_SIV;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   overriding
   procedure Encrypt(This             : in out AEAD_SIV;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   overriding
   function Decrypt_And_Verify(This                   : in out AEAD_SIV;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;

   overriding
   function Decrypt_And_Verify(This                   : in out AEAD_SIV;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;

private

   type AEAD_SIV is new AE.AE_Scheme and AEAD.AEAD_Scheme with
      record
         CTR_Key  : Key_Type;
         Result   : Block;
      end record;

end Crypto.Symmetric.AEAD_SIV;