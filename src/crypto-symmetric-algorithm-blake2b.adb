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

package body Crypto.Symmetric.Algorithm.Blake2b is


procedure Compress( Number : in Integer;
					i1    : in DW_Block512;
                    i2    : in DW_Block512;
                    Output: out DW_Block512) is
begin
	Hash(to_Bytes(i1)&to_Bytes(i2), Output);
end Compress;

-- procedure resetState is
-- begin
-- 	null;
-- end resetState;

procedure Init(This 		: in out Blake2b_Context) is
begin
	Blake2bInit(This.Context, 64);
end Init;

procedure Round(This 	: in out 	Blake2b_Context;
    	   Message_Block: in 		DW_Block512) is
begin
	blake2bUpdate(This.Context, To_Bytes(Message_Block));
end Round;

function Final_Round(This 		    : in out Blake2b_Context;
                     Last_Message_Block  : DW_Block512;
                	 Last_Message_Length : Message_Block_Length512)
                     			return DW_Block512 is
	output_bytes : bytes(0..63);
begin
	blake2bUpdate(This.Context, To_Bytes(Last_Message_Block));
	blake2bFinal(This.Context, output_bytes);
	return TO_DW_block512(output_bytes);
end Final_Round;

procedure Hash(Message : in Bytes;  Hash_Value : out DW_Block512) is
	ctx:	Blake2bState;
	output_bytes : bytes(0..63);
begin
	Blake2bInit(ctx, 64);
	Blake2bUpdate(ctx, Message);
	blake2bFinal(ctx, output_bytes);
	Hash_Value := TO_DW_block512(output_bytes);
end Hash;

procedure Hash(Message : in String; Hash_Value : out DW_Block512) is
begin
	Hash(to_Bytes(Message), Hash_Value);
end Hash;

procedure F_Hash(Filename : in String; Hash_Value : out DW_Block512) is
begin
	--temporary, not correct!
	Hash(to_Bytes(Filename), Hash_Value);
end F_Hash;


procedure blake2bUpdate(S 			: in out Blake2bState;
						input		: in Bytes) is
	inlen : Natural := input'Length;
	left, fill : Natural;
	inOffset : Natural := 0;
begin
	while inlen > 0 loop
		left := S.buflen;
		fill := 2 * BLAKE2B_BLOCKBYTES - left;
		
		if inlen > fill then
			s.buf(s.buf'first + left..s.buf'first + left + fill -1) := input(input'first+inOffset..input'first+inOffset+fill -1);
			S.buflen := S.buflen + fill;
			blake2bIncrementCounter(S, BLAKE2B_BLOCKBYTES);
			blake2bCompress(S, S.buf(0..BLAKE2B_BLOCKBYTES -1));
			S.buf(0..BLAKE2B_BLOCKBYTES -1) := S.buf(BLAKE2B_BLOCKBYTES..2*BLAKE2B_BLOCKBYTES-1);
			S.buflen := S.buflen - BLAKE2B_BLOCKBYTES;
			inOffset := inOffset + fill;
			inlen := inlen - fill;
		else
			s.buf(s.buf'first + left..s.buf'first + left + inlen -1) := input(input'first+inOffset..input'first+inOffset+inlen -1);
			S.buflen := S.buflen + inlen;
			inOffset := inOffset + inlen;
			inlen := inlen - inlen;
		end if;	

	end loop;
end blake2bUpdate;


procedure blake2bFinal(S 			: in out Blake2bState;
						output		: out Bytes) is
	buffer : Bytes(output'Range);
begin

	if S.buflen > BLAKE2B_BLOCKBYTES then
		blake2bIncrementCounter(S, BLAKE2B_BLOCKBYTES);
		blake2bCompress(S, S.buf);
		S.buflen := S.buflen - BLAKE2B_BLOCKBYTES;
		S.buf(0..S.buflen-1) := S.buf(BLAKE2B_BLOCKBYTES .. BLAKE2B_BLOCKBYTES +S.buflen -1);
	end if;
	blake2bIncrementCounter(S, S.buflen);
	S.f(0):= (16#FFFFFFFFFFFFFFFF#);
	S.buf(s.buflen..2 * BLAKE2B_BLOCKBYTES - S.buflen -1) := (others=>0);
	-- printState(S);
	blake2bCompress(S, S.buf);
	-- printState(S);
	buffer := to_Bytes(S.h);
	output := buffer(buffer'first..buffer'first + output'length -1);

end;



procedure blake2bCompress(S 			: in out Blake2bState;
						  block			: in Bytes) is
	m : DWords(0..15);
	v : DWords(0..15);
begin
	
	--  consider load() function
	m := to_Dwords(block(0..127));
	v(0..7) := s.h;
	v(8..11) := blake2bIV(0..3);
	v(12..13) := S.t(0..1) xor blake2bIV(4..5);
	v(14..15) := S.f(0..1) xor blake2bIV(6..7);
	
	for i in Natural range 0..15 loop
		v(i):= reverseDword(v(i));
		m(i) := reverseDword(m(i));
	end loop;

	Round_Blake2(0, v, m);
	Round_Blake2(1, v, m);
	Round_Blake2(2, v, m);
	Round_Blake2(3, v, m);
	Round_Blake2(4, v, m);
	Round_Blake2(5, v, m);
	Round_Blake2(6, v, m);
	Round_Blake2(7, v, m);
	Round_Blake2(8, v, m);
	Round_Blake2(9, v, m);
	Round_Blake2(10, v, m);
	Round_Blake2(11, v, m);

	for i in Natural range 0..15 loop
		v(i):= reverseDword(v(i));
	end loop;
	

	S.h := S.h xor v(0..7) xor v(8..15);

end;

procedure G (r : in Natural;
			 i : in Natural;	
			 a : in out DWord;
			 b : in out DWord;
			 c : in out DWord;
			 d : in out DWord;
			 m : in DWords) is 
begin
	
	a := a + b + (((m(blake2bSigma(r,2*i+0)))));
	d := Rotate_Right(d xor a, 32);
	c := c + d;
	b := Rotate_Right(b xor c, 24);

	a := a + b + (((m(blake2bSigma(r,2*i+1)))));
	d := Rotate_Right(d xor a, 16);
	c := c+d;
	b := Rotate_Right(b xor c, 63);

end G; pragma inline(G);

procedure Round_Blake2( r: in Natural;
						v: in out DWords;
			 			m : in DWords) is
begin
	G(r, 0, v(0),v(4),v(8),v(12), m);
	G(r, 1, v(1),v(5),v(9),v(13), m);
	G(r, 2, v(2),v(6),v(10),v(14), m);
	G(r, 3, v(3),v(7),v(11),v(15), m);

	G(r, 4, v(0),v(5),v(10),v(15), m);
	G(r, 5, v(1),v(6),v(11),v(12), m);
	G(r, 6, v(2),v(7),v(8),v(13), m);
	G(r, 7, v(3),v(4),v(9),v(14), m);
end Round_Blake2; pragma inline(Round_Blake2);

end  Crypto.Symmetric.Algorithm.Blake2b;





