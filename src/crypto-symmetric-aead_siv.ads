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
