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

package body Crypto.Symmetric.Algorithm.Blowfish.Oneway is

   procedure Prepare_Oneway_Key128
     (Key      : in B_Block128;
      Cipherkey: out Cipherkey_Oneway_Blowfish128) is

   begin
      Prepare_Key128(Key,Cipherkey_Blowfish128(Cipherkey));
   end Prepare_Oneway_Key128;



   procedure Encrypt_Oneway128(Cipherkey  : in  Cipherkey_Oneway_Blowfish128;
                               Plaintext  : in  B_Block64;
                               Ciphertext : out B_Block64) is
   begin
      Encrypt128(Cipherkey_Blowfish128(Cipherkey), Plaintext, Ciphertext);
   end  Encrypt_Oneway128;

end Crypto.Symmetric.Algorithm.Blowfish.Oneway;
