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

with  Crypto.Symmetric.Oneway_Blockcipher;

generic
   with package C is new Crypto.Symmetric.Oneway_Blockcipher(<>);

   with function "xor" (Left, Right : C.Block) return C.Block is <>;

package Crypto.Symmetric.Mode.Oneway_OFB is
   use C;

   procedure Init(Key : in Key_Type; Initial_Value : in Block);

   procedure Encrypt(Plaintext  : in Block; Ciphertext : out Block);
   procedure Decrypt(Ciphertext : in Block; Plaintext  : out Block);

   procedure Set_IV(Initial_Value : in Block);

   procedure Next_Block(Keystream : out Block);

   ---------------------------------------------------------------------------
   -----------------------------PRIVATE---------------------------------------
   ---------------------------------------------------------------------------

private
   pragma Inline (Init, Next_Block, Set_IV);
   pragma Optimize (Time);

   end  Crypto.Symmetric.Mode.Oneway_OFB;


