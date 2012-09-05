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


package body Crypto.Symmetric.Oneway_Blockcipher is

   ---------------------------------------------------------------------------

   Ck : Cipherkey_Type;

   ---------------------------------------------------------------------------

   procedure  Prepare_Key(Key : in Key_Type) is
   begin
      Prepare_Key(Key, Ck);
   end Prepare_Key;

   ---------------------------------------------------------------------------

   procedure Encrypt(Plaintext : in Block; Ciphertext : out Block) is
   begin
      Encrypt(Ck, Plaintext, Ciphertext);
   end Encrypt;

   ---------------------------------------------------------------------------
 
   function To_Block(Byte_Array : Crypto.Types.Bytes) return Block is
   begin
      if Byte_Array'Size  /= Block'Size then
         raise Constraint_Error with "Key length doesn't match.";
      end if;
      return To_Block_Type(Byte_Array);
   end To_Block; 
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(B : Block) return Crypto.Types.Bytes is
   begin
      return Block_To_Bytes(B);
   end To_Bytes;
   ---------------------------------------------------------------------------

end Crypto.Symmetric.Oneway_Blockcipher;

