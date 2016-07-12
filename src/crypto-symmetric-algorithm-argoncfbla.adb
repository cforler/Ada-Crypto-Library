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

with Crypto.Symmetric.Algorithm.Blake2b_Utils; use Crypto.Symmetric.Algorithm.Blake2b_Utils;

package body Crypto.Symmetric.Algorithm.ArgonCFBla is

-- procedure resetState is
-- begin
-- 	null;
-- end resetState;


procedure Compress( Number : in Integer;
					i1    : in DW_Block8192;
                    i2    : in DW_Block8192;
                    Output: out DW_Block8192) is
	
	r : DW_Block8192 := i1 xor i2; 
	r_tmp : constant DW_Block8192 := r;
   begin
	
	-- Apply Blake2 on columns of 64-bit words: (0,1,...,15) , then (16,17,..31)... finally (112,113,...127)
	
	
	  for i in r'range loop
		r(i) := reverseDword(r(i));
	  end loop;

	for i in Natural range 0..7 loop
		Round_Blake2(r(16 * i), r(16 * i + 1), r(16 * i + 2), r(16 * i + 3),
                r(16 * i + 4), r(16 * i + 5), r(16 * i + 6), r(16 * i + 7),
                r(16 * i + 8), r(16 * i + 9), r(16 * i + 10), r(16 * i + 11),
                r(16 * i + 12), r(16 * i + 13), r(16 * i + 14), r(16 * i + 15));
	end loop;
		
     -- Apply Blake2 on rows of 64-bit words: (0,1,16,17,...112,113), then (2,3,18,19,...,114,115).. finally (14,15,30,31,...,126,127)
    for i in Natural range 0..7 loop
        Round_Blake2(r(2 * i), r(2 * i + 1), r(2 * i + 16), r(2 * i + 17),
                r(2 * i + 32), r(2 * i + 33), r(2 * i + 48), r(2 * i + 49),
                r(2 * i + 64), r(2 * i + 65), r(2 * i + 80), r(2 * i + 81),
                r(2 * i + 96), r(2 * i + 97), r(2 * i + 112), r(2 * i + 113));
    end loop;
	
	for i in r'range loop
		r(i) := reverseDword(r(i));
	  end loop;

    Output := r xor r_tmp;	

   end Compress;

procedure G (a : in out DWord;
			 b : in out DWord;
			 c : in out DWord;
			 d : in out DWord) is 
begin
	a := fblamka(a, b);
	d := Rotate_Right(d xor a, 32);
	c := fblamka(c, d);
	b := Rotate_Right(b xor c, 24);
	a := fblamka(a, b);
	d := Rotate_Right(d xor a, 16);
	c := fblamka(c, d);
	b := Rotate_Right(b xor c, 63);

end G;


procedure Round_Blake2(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: in out Dword) is
begin
	G(v0,v4,v8,v12);
	G(v1,v5,v9,v13);
	G(v2,v6,v10,v14);
	G(v3,v7,v11,v15);

	G(v0,v5,v10,v15);
	G(v1,v6,v11,v12);
	G(v2,v7,v8,v13);
	G(v3,v4,v9,v14);
end Round_Blake2;

end  Crypto.Symmetric.Algorithm.ArgonCFBla;