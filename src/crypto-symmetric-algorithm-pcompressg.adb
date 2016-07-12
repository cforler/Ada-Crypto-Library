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

package body Crypto.Symmetric.Algorithm.PCompressG is

-- procedure resetState is
-- begin
-- 	null;
-- end resetState;


procedure Compress( Number : in Integer;
					i1    : in DW_Block512;
                    i2    : in DW_Block512;
                    Output: out DW_Block512) is
	
	r : DW_Block1024; 
   begin
	  r := DW_Block1024(Dwords(i1) & Dwords(i2));
      for i in Natural range 0..15 loop
		r(i) := reverseDword(r(i));
	  end loop;

      Round_Blake2(r);

      for i in Natural range 0..15 loop
		r(i) := reverseDword(r(i));
	  end loop;
	  
	  Output := DW_Block512(Dwords(r(0..7))) xor DW_Block512(Dwords(r(8..15))) ;

   end Compress;

procedure G (a : in out DWord;
			 b : in out DWord;
			 c : in out DWord;
			 d : in out DWord) is 
begin
	a := a + b;
	d := Rotate_Right(d xor a, 32);
	c := c + d;
	b := Rotate_Right(b xor c, 24);
	a := a+b;
	d := Rotate_Right(d xor a, 16);
	c := c+d;
	b := Rotate_Right(b xor c, 63);

end G;

procedure Round_Blake2(r: in out DW_Block1024) is
begin
	G(r(0),r(4),r(8),r(12));
	G(r(1),r(5),r(9),r(13));
	G(r(2),r(6),r(10),r(14));
	G(r(3),r(7),r(11),r(15));

	G(r(0),r(5),r(10),r(15));
	G(r(1),r(6),r(11),r(12));
	G(r(2),r(7),r(8),r(13));
	G(r(3),r(4),r(9),r(14));
end Round_Blake2;

end  Crypto.Symmetric.Algorithm.PCompressG;





