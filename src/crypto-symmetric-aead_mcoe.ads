with Crypto.Symmetric.AE;
with Crypto.Symmetric.AE.AD;
with Crypto.Types.Nonces;

with Crypto.Symmetric.Tweakable_Blockcipher;
with Crypto.Types;

use Crypto.Types;

generic
   type Block is private;
   type Key_Type is private;

   with package N is new Crypto.Types.Nonces(Block);
   with package Tweakable_Blockcipher is new Crypto.Symmetric.Tweakable_Blockcipher(Block  => Block, Key_Type   => Key_Type, Tweak_Type => Block);
   type TB_Type is new Tweakable_Blockcipher.TB_Interface with private;

   with function "xor"(Left, Right : Block) return Block is <>;
   with function To_Block_Type(B : in Bytes) return Block is <>;
   with function To_Byte_Word (X: Word) return Byte_Word;         -- to convert a blocklength into bytes

   with function To_Bytes(B : in Block) return Bytes;

package Crypto.Symmetric.AEAD_McOE is

   package AE is new Crypto.Symmetric.AE(Key_Type => Key_Type,
                                         Block    => Block,
                                         N        => N);
   use AE;

   package AEAD is new AE.AD;

   type AEAD_McOE is new AE.AE_Scheme and AEAD.AEAD_Scheme with private;
   
   
   Invalid_Ciphertext_Error : exception;

   ---------------------------------------------
   ---- overriding functions and procedures ----
   ---------------------------------------------

   overriding
   procedure Init_Encrypt(This   : out    AEAD_McOE;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class);

   overriding
   procedure Init_Decrypt(This        : out AEAD_McOE;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block);

   overriding
   procedure Encrypt(This             : in out AEAD_McOE;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   overriding
   procedure Encrypt(This             : in out AEAD_McOE;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   function Decrypt_And_Verify(This                   : in out AEAD_McOE;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;

   function Decrypt_And_Verify(This                   : in out AEAD_McOE;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;
private

   type AEAD_McOE is new AE.AE_Scheme and AEAD.AEAD_Scheme with
      record
         Nonce_Value : Block;
         Tweak       : Block;
	 TB : TB_Type;	 
      end record;
end Crypto.Symmetric.AEAD_McOE;
