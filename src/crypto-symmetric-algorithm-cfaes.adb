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

-- All the procedures of this package based on FIPS 180-2

with Crypto.Symmetric.Algorithm.AES; use Crypto.Symmetric.Algorithm.AES;

package body Crypto.Symmetric.Algorithm.CFAES is

-- procedure resetState is
-- begin
--   null;
-- end resetState;


procedure Compress( Number : in Integer; 
                    i1    : in DW_Block8192;
                    i2    : in DW_Block8192;
                    Output: out DW_Block8192) is
	aesKey   : Cipherkey_AES128;
  keyBlock : constant B_Block128 := (others=>0);

	type BBlock128_Array is array (Integer range <>) of B_Block128;
	
  state	 : BBlock128_Array(0..127);
	tmpBytes : Bytes(0..2047);
	tmpBlock : B_Block128 := (others=>0);
  
  begin
   Prepare_key128(keyBlock, aesKey);

   tmpBytes(0..1023) := To_Bytes(i1);
   tmpBytes(1024..2047) := To_Bytes(i2);

   for i in 0..127 loop
   	state(i) := to_B_Block128(tmpBytes(i*16..(i+1)*16-1));
	tmpBlock := tmpBlock xor state(i);
   end loop;

   for i in 0..127 loop
       Encrypt128_4rounds(aesKey, tmpBlock xor state(i), state(i));
   end loop;
	
	tmpBlock := (others=>0);

   for i in 0..127 loop
	tmpBlock := tmpBlock xor state(i);
   end loop;

	for i in 0..63 loop
   		Encrypt128_4rounds(aesKey, tmpBlock xor state(i), state(i));
   		tmpBytes(i*16..(i+1)*16-1) := To_Bytes(state(i));
   end loop;

    Output := to_DW_Block8192(tmpBytes(0..1023));
    
   end Compress;

end  Crypto.Symmetric.Algorithm.CFAES;





