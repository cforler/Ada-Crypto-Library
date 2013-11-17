with Crypto.Symmetric.AE;
with Crypto.Types.Nonces;
with Crypto.Symmetric.Blockcipher;
with Crypto.Types; use Crypto.Types;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>);
   with package N is new Crypto.Types.Nonces(BC.Block);

   with function "xor" (Left, Right : in BC.Block) return BC.Block is <>;
   with function To_Block_Type (B : Bytes) return BC.Block;
   with function To_Bytes (B : BC.Block) return Bytes;
   with function Shift_Left (Value: BC.Block; Amount: Natural) return BC.Block;  -- Used to generate irreducible polynomials L(1..31).
   with function Shift_Right (Value: BC.Block; Amount: Natural) return BC.Block; -- Used to generate the irreducible polynomial L(-1).
   with function To_Byte_Word (X: Word) return Byte_Word;                        -- Used to convert Blockcounter into Bytes.

package Crypto.Symmetric.AE_OCB3 is
   use BC;

   package AE is new Crypto.Symmetric.AE(Key_Type => Key_Type,
                                         Block    => Block,
                                         N        => N);
   use AE;

   type AE_OCB is new AE.AE_Scheme with private;
   Bytes_Per_Block : constant Positive := Block'Size / 8;

   ---------------------------------------------
   ---- overriding functions and procedures ----
   ---------------------------------------------

   overriding
   procedure Init_Encrypt(This   : out    AE_OCB;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class);

   overriding
   procedure Init_Decrypt(This        : out AE_OCB;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block);

   overriding
   procedure Encrypt(This             : in out AE_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   overriding
   function Decrypt_And_Verify(This                   : in out AE_OCB;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;

   ---------------------------------------------
   ---- additional functions and procedures ----
   ---------------------------------------------

   -- Add parameter Bytes_Of_N_Read
   not overriding
   procedure Init_Encrypt(This   : out    AE_OCB;
                          Key    : in     Key_Type;
                          N_Init           : in out N.Nonce'Class;
                          Bytes_Of_N_Read  : in     Positive;
                          Taglen : in     Positive);

   -- Add parameter Bytes_Of_N_Read
   not overriding
   procedure Init_Decrypt(This                : out    AE_OCB;
                          Key                 : in     Key_Type;
                          N_Init              : in     Block;
                          Bytes_Of_N_Read     : in     Positive;
                          Taglen              : in     Positive);
   
   procedure Hash(Key             : in	Key_Type;
                  Associated_data : in	Callback_Reader;
                  Sum		  : out B_Block128);

private
   type Block_Array is array (-1..31) of BC.Block;
   subtype Taglength_Range is Positive range 1..Bytes_Per_Block;
   
   type AE_OCB is new AE.AE_Scheme with
      record
         Nonce_Value : BC.Block;
         Offset      : BC.Block;
         L_Array     : Block_Array;
         Taglen      : Taglength_Range;
      end record;

   Blocklength_Not_Supported: exception;
   Noncelength_Not_Supported: exception;

end Crypto.Symmetric.AE_OCB3;
