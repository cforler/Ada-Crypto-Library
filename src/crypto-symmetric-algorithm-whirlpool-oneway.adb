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


package body Crypto.Symmetric.Algorithm.Whirlpool.Oneway is

   ---------------------------------------------------------------------------

   procedure Prepare_Key(Key       : in  DW_Block256;
                         Cipherkey : out Cipherkey_Whirlpool) is
   begin
      Cipherkey.Left_Key  := DW_Block128(Key(0..1));
      Cipherkey.Right_Key := DW_Block128(Key(2..3));
   end;

   ---------------------------------------------------------------------------

   procedure Encrypt(Cipherkey  : in  Cipherkey_Whirlpool;
                     Plaintext  : in  DW_Block256;
                     Ciphertext : out DW_Block256) is
     M : DWords(DW_Block512'Range);
     H : DW_Block512 := (others =>0); -- Init(H)

   begin
      M(0..1) := DWords(Cipherkey.Left_Key);
      M(2..5) := DWords(Plaintext);
      M(6..7) := DWords(Cipherkey.Right_Key);
      Round(DW_Block512(M),H);
      Ciphertext := DW_Block256(DWords(H(0..3)) xor DWords(H(4..7)));
   end Encrypt;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.Whirlpool.Oneway;

