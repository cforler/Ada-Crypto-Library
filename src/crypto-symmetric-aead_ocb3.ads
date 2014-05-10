-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Crypto.Symmetric.AE;
with Crypto.Symmetric.AE.AD;
with Crypto.Types.Nonces;
with Crypto.Symmetric.Blockcipher;
with Crypto.Types; use Crypto.Types;
with Crypto.Debug_Put;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>);
   with package N is new Crypto.Types.Nonces(BC.Block);

   with function "xor" (Left, Right : in BC.Block) return BC.Block is <>;
   with function To_Block_Type (B : Bytes) return BC.Block;
   with function To_Bytes (B : BC.Block) return Bytes;
   with function Shift_Left (Value: BC.Block; Amount: Natural) return BC.Block;  -- Used to generate irreducible polynomials L(1..31).
   with function Shift_Right (Value: BC.Block; Amount: Natural) return BC.Block; -- Used to generate the irreducible polynomial L(-1).
   with function To_Byte_Word (X: Word) return Byte_Word;                        -- Used to convert Blockcounter into Bytes.

package Crypto.Symmetric.AEAD_OCB3 is
   use BC;

   package AE is new Crypto.Symmetric.AE(Key_Type => Key_Type,
                                         Block    => Block,
                                         N        => N);

   package AEAD is new AE.AD;
   use AE;

   package Error_Output is new Crypto.Debug_Put(b => false);

   type AEAD_OCB is new AE.AE_Scheme and AEAD.AEAD_Scheme with private;

   Bytes_Per_Block : constant Positive := Block'Size / 8;

   ---------------------------------------------
   ---- overriding functions and procedures ----
   ---------------------------------------------

   overriding
   procedure Init_Encrypt(This   : out    AEAD_OCB;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class);

   overriding
   procedure Init_Decrypt(This        : out AEAD_OCB;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block);

   overriding
   procedure Encrypt(This             : in out AEAD_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);


   overriding
   function Decrypt_And_Verify(This                   : in out AEAD_OCB;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean;

   overriding
   procedure Encrypt(This             : in out AEAD_OCB;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer);

   overriding
   function Decrypt_And_Verify(This                   : in out AEAD_OCB;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                                  return Boolean;

   --------------------------------------------------
   -- Encryption / Decryption with Associated Data --
   --------------------------------------------------

   procedure Encrypt(This             : in out AEAD_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer;
                     Read_AD	      : in     Callback_Reader);

   procedure Encrypt(This             : in out AEAD_OCB;
                     Plaintext        : in     Bytes;
                     Ciphertext       : out    Bytes;
                     AD	      	      : in     Bytes);

   function Decrypt_And_Verify(This                   : in out AEAD_OCB;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer;
                               Read_AD	      	      : in     Callback_Reader)
                               return Boolean;

   function Decrypt_And_Verify(This                   : in out AEAD_OCB;
                               Ciphertext             : in     Bytes;
                               Plaintext              : out    Bytes;
                               AD	      	      : in     Bytes)
                               return Boolean;

   ---------------------------------------------
   ---- additional functions and procedures ----
   ---------------------------------------------

   -- Add parameter Bytes_Of_N_Read
   not overriding
   procedure Init_Encrypt(This   : out    AEAD_OCB;
                          Key    : in     Key_Type;
                          N_Init           : in out N.Nonce'Class;
                          Bytes_Of_N_Read  : in     Positive;
                          Taglen : in     Positive);

   -- Add parameter Bytes_Of_N_Read
   not overriding
   procedure Init_Decrypt(This                : out    AEAD_OCB;
                          Key                 : in     Key_Type;
                          N_Init              : in     Block;
                          Bytes_Of_N_Read     : in     Positive;
                          Taglen              : in     Positive);

   procedure EmptyReader(B : out Bytes; Count: out Natural);


private

   --"Hashing" for Associated Data
   function Hash_AD(This     	  : in  AEAD_OCB;
                    Read_AD 	  : in	Callback_Reader) return Block;




   type Block_Array is array (-1..31) of BC.Block;
   subtype Taglength_Range is Positive range 1..Bytes_Per_Block;

   type AEAD_OCB is new AE.AE_Scheme and AEAD.AEAD_Scheme with
      record
         Nonce_Value : BC.Block;
         Offset      : BC.Block;
         L_Array     : Block_Array;
         Taglen      : Taglength_Range;
      end record;

   Blocklength_Not_Supported: exception;
   Noncelength_Not_Supported: exception;

end Crypto.Symmetric.AEAD_OCB3;
