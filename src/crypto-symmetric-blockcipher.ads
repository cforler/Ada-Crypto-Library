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
with Crypto.Types;

generic
   type Block is private;
   type Key_Type is private;
   type Cipherkey_Type is private;

   with procedure Prepare_Key(Key : in Key_Type;
                              Cipherkey : out Cipherkey_Type) is <>;

   with procedure Encrypt(Cipherkey  : in Cipherkey_Type;
                          Plaintext  : in Block;
                          Ciphertext : out Block) is <>;

   with procedure Decrypt(Cipherkey  : in Cipherkey_Type;
                          Ciphertext : in Block;
                          Plaintext  : out Block) is <>;  
   
   with function To_Key_Type   (B : Crypto.Types.Bytes) return Key_Type is <>;
   with function To_Block_Type (B : Crypto.Types.Bytes) return Block is <>;
   with function Block_To_Bytes(B : Block) return Crypto.Types.Bytes is <>;

package Crypto.Symmetric.Blockcipher is

   procedure Prepare_Key(Key    : in Key_Type);

   procedure Encrypt(Plaintext  : in Block; Ciphertext : out Block);

   procedure Decrypt(Ciphertext : in Block; Plaintext  : out Block);
   
   -- Function raise Constraint Error if Byte_Array'Size /= Key_Type'Size
   function To_Key(Byte_Array : Crypto.Types.Bytes) return Key_Type;
   
   -- Function raise Constraint Error if Byte_array'Size /= Block'Size
   function To_Block(Byte_Array : Crypto.Types.Bytes) return Block;
   
   function To_Bytes(B : Block) return Crypto.Types.Bytes;
   
private
   pragma Inline(Prepare_Key, Encrypt, Decrypt);
   pragma Optimize (Time);
end Crypto.Symmetric.Blockcipher;


