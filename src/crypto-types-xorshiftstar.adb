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

with Ada.Unchecked_Conversion;

with Crypto.Symmetric.Algorithm.Blake2b_Utils; use Crypto.Symmetric.Algorithm.Blake2b_Utils;

package body Crypto.Types.XORShiftSTAR is

	procedure init(Seed	: Bytes) is
	use HF;
	hash : Hash_Type;
	hash2 : Hash_Type;
	begin
      	HF.Hash(Seed,hash);
		HF.Hash(To_Bytes(hash),hash2);
		r := To_DW_Block1024(To_Bytes(hash) & To_Bytes(hash2));
		p := 0;
	end init;


	function getValue(bits : Natural) return Integer is
		s0 : DWord := r(p);
		s1 : DWord := r((p+1) mod 16);
		final_const : constant DWord := 1181783497276652981;
		result : DWord;
		function Cast is new Ada.Unchecked_Conversion (DWord, Natural);

	begin
		p := (p+1) mod 16;
		s1 := reverseDword(s1);
		s1 := s1 xor (Shift_Left(s1, 31));
		s1 := s1 xor (Shift_Right(s1, 11));
		s1 := reverseDword(s1);

		s0 := reverseDword(s0);
		s0 := s0 xor (Shift_Right(s0, 30));
		s0 := reverseDword(s0);

		result := s0 xor s1;

      	r(p) := result;
      	result := reverseDword(result)*final_const;
      	result := Shift_Right(result, 64-bits);

		return Cast(result);
	end getValue;


end Crypto.Types.XORShiftSTAR;