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


package Crypto.Symmetric.Algorithm.SHA1.Oneway is


   type Cipherkey_SHA1 is private;

   -- oneway-blockcipher API


   procedure Prepare_Key(Key       : in  W_Block256;
                         Cipherkey : out Cipherkey_SHA1);

   procedure Encrypt(Cipherkey  : in  Cipherkey_SHA1;
                     Plaintext  : in  W_Block160;
                     Ciphertext : out W_Block160);

   ---------------------------------------------------------------------------

  private

   type Cipherkey_SHA1 is record
      Left_Key  : W_Block192;
      Right_Key : W_Block160;
   end record;

   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.SHA1.Oneway;

