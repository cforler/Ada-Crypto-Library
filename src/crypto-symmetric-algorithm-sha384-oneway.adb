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


package body Crypto.Symmetric.Algorithm.SHA384.Oneway is

   procedure Prepare_Key(Key       : in  DW_Block256;
                         Cipherkey : out Cipherkey_SHA384) is
      H : DW_Block512;
      M : DWords(DW_Block1024'Range);
      K2: DW_Block256;
   begin
      K2(0..1) := Key(2..3);
      K2(2..3) := Key(0..1);
      Init(H);

      M( 0.. 3)  := DWords(Key);
      M( 4.. 7)  := Dwords(K2);
      M( 8..11)  := Dwords(K2);
      M(12..15)  := Dwords(Key);
      Round(DW_Block1024(M),H);
      Cipherkey.Left_Key  := DWords(H(0..4));

      M( 0.. 3)  := DWords(K2);
      M( 4.. 7)  := DWords(Key);
      M( 8..11)  := Dwords(K2);
      M(12..15)  := Dwords(Key);
      Round(DW_Block1024(M),H);
      Cipherkey.Right_Key := DWords(H(0..4));
      
      
      pragma Warnings(Off,"useless");
      M  := (others => 0);
      H  := (others => 0);
      pragma Warnings(On,"useless");
   end Prepare_Key;

   ---------------------------------------------------------------------------

   procedure Encrypt(Cipherkey  : in  Cipherkey_SHA384;
                     Plaintext  : in  DW_Block384;
                     Ciphertext : out DW_Block384) is
      M : DWords(DW_Block1024'Range);
      H : DW_Block512;
   begin
      Init(H);
      M( 0.. 4) := Cipherkey.Left_Key;
      M( 5..10) := DWords(Plaintext);
      M(11..15) := Cipherkey.Right_Key;
      Round(DW_Block1024(M), H);
      Ciphertext := DW_Block384(H(0..5));
   end Encrypt;


end  Crypto.Symmetric.Algorithm.SHA384.Oneway;
