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


package Crypto.Symmetric.Algorithm.Whirlpool.Oneway is

   type Cipherkey_Whirlpool is private;

   ---------------------------------------------------------------------------

   -- oneway-blockcipher API

   procedure Prepare_Key(Key       : in  DW_Block256;
                         Cipherkey : out Cipherkey_Whirlpool);


   procedure Encrypt(Cipherkey  : in  Cipherkey_Whirlpool;
                     Plaintext  : in  DW_Block256;
                     Ciphertext : out DW_Block256);


   ---------------------------------------------------------------------------
   -----------------------------PRIVATE PART----------------------------------
   ---------------------------------------------------------------------------

private
   type Cipherkey_Whirlpool is record
      Left_Key  : DW_Block128;
      Right_Key : DW_Block128;
   end record;

   pragma Inline   (Prepare_Key);
   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.Whirlpool.Oneway;
