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
