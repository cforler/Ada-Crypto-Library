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

-- This SHA-512 implementation is based on fips-180-2

with Crypto.Symmetric.Algorithm.Blake2b_Utils; use Crypto.Symmetric.Algorithm.Blake2b_Utils;

package Crypto.Symmetric.Algorithm.Blake2b1 is


type Generic_Context is Interface;
   type Blake2b_Context is new Generic_Context with
      record
         Context : Blake2bState;
         Hash_Value : DW_Block512;
      end record;


procedure resetState(Ctx : in out Blake2b_Context);

-- compress API

procedure Compress( Ctx : in out Blake2b_Context;
					Number : in Integer;
                    i1    : in DW_Block512;
                    i2    : in DW_Block512;
                    Output: out DW_Block512);
-- internal functions

procedure G (r : in Natural;
			 i : in Natural;	
			 a : in out DWord;
			 b : in out DWord;
			 c : in out DWord;
			 d : in out DWord;
			 m : in DWords);

procedure Round_Blake2( r: in Natural;
						v: in out Dwords;
			 			m : in DWords);

procedure blake2bCompress(Number : in Integer;
							S 			: in out Blake2bState;
						  block			: in Bytes);

private
   
   pragma Inline (Round_Blake2, G);
   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.Blake2b1;


