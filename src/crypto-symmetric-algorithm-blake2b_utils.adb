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
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MACast2
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

-- All the procedures of this package based on FIPS 180-2

with Crypto.Types.Output; use Crypto.types.Output;
with Ada.Unchecked_Conversion;

with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

package body Crypto.Symmetric.Algorithm.Blake2b_Utils is

function fblamka(x : in DWord;
				 y : in Dword) return Dword is
	function Cast is new Ada.Unchecked_Conversion (Word, DWord);
	function Cast2 is new Ada.Unchecked_Conversion (DWord, Word);
	tmpx : constant Dword := Cast(Cast2(x));
	tmpy : constant Dword := Cast(Cast2(y));
begin
	return Shift_Left(tmpx*tmpy, 1) + x + y;
end fblamka; pragma Inline (fblamka);

procedure blake2bIncrementCounter(	S 		: in out Blake2bState;
									inc		: in Natural) is
    function Cast is new Ada.Unchecked_Conversion (Natural, Dword);
begin
	S.t(0) := reverseDword(S.t(0));
	S.t(1) := reverseDword(S.t(1));

	S.t(0) := S.t(0) + Cast(inc);
	if  S.t(0) < Cast(inc) then
		 S.t(1) := Cast(1);
	else
		S.t(1) := Cast(0);
	end if;

	S.t(0) := reverseDword(S.t(0));
	S.t(1) := reverseDword(S.t(1));
end; pragma Inline (blake2bIncrementCounter);

procedure Blake2bInit(	S 		: in out Blake2bState;
						outlen	: in Integer) is
	P : Blake2bParam;
begin
	P.digest_length := outlen;
  	P.key_length    := 0;
  	P.fanout        := 1;
  	P.depth         := 1;

	-- Keep Eye on Byte Order
  	P.leaf_length	:= 0;
  	P.node_offset 	:= 0;

  	P.node_depth    := 0;
  	P.inner_length  := 0;

  	P.reserved 		:= (others=>0);
  	P.salt 			:= (others=>0);
  	P.personal 		:= (others=>0);

  	Blake2bInitParam(S, P);

end Blake2bInit;

procedure Blake2bInitParam	(	S 		: in out Blake2bState;
								p		: in Blake2bParam) is
	function NaturalToByte is new Ada.Unchecked_Conversion (Natural, Byte);
	function NaturalToWord is new Ada.Unchecked_Conversion (Natural, Word);
	function NaturalToDWord is new Ada.Unchecked_Conversion (Natural, DWord);
    tmpBD		: Byte_DWord;

begin
	S.h 		:=(others=>0);
    S.t 		:=(others=>0);
    S.f 		:=(others=>0);
    S.buf 		:=(others=>0);
    S.buflen	:= 0;
    S.lastNode	:=(0);

	S.h := blake2bIV;

	tmpBD(0):= NaturalToByte(p.digest_length);
	tmpBD(1):= NaturalToByte(p.key_length);
	tmpBD(2):= NaturalToByte(p.fanout);
	tmpBD(3):= NaturalToByte(p.depth);
	tmpBD(4..7) := to_Bytes(NaturalToWord(p.leaf_length));
	S.h(0) := S.h(0) xor To_DWord(tmpBD);

	S.h(1) := S.h(1) xor NaturalToDWord(p.node_offset);
	
	tmpBD(0):= NaturalToByte(p.node_depth);
	tmpBD(1):= NaturalToByte(p.inner_length);
	tmpBD(2..7):= p.reserved(0..5);
	S.h(2) := S.h(2) xor To_DWord(tmpBD);

	S.h(3) := S.h(3) xor To_DWord(p.reserved(6..13));	

	S.h(4) := S.h(4) xor To_DWord(p.salt(0..7));	
	S.h(5) := S.h(5) xor To_DWord(p.salt(8..15));	

	S.h(6) := S.h(6) xor To_DWord(p.personal(0..7));	
	S.h(7) := S.h(7) xor To_DWord(p.personal(8..15));	

end Blake2bInitParam;

function reverseDword(input : Dword) return Dword is
	returnValue : Dword := 0;
begin
	if System.Default_Bit_Order = System.Low_Order_First then 
	returnValue := returnValue xor shift_left(input and 16#00000000000000ff#, 56);
	returnValue := returnValue xor shift_left(input and 16#000000000000ff00#, 40);
	returnValue := returnValue xor shift_left(input and 16#0000000000ff0000#, 24);
	returnValue := returnValue xor shift_left(input and 16#00000000ff000000#, 8);
	returnValue := returnValue xor shift_right(input and 16#000000ff00000000#, 8);
	returnValue := returnValue xor shift_right(input and 16#0000ff0000000000#, 24);
	returnValue := returnValue xor shift_right(input and 16#00ff000000000000#, 40);
	returnValue := returnValue xor shift_right(input and 16#ff00000000000000#, 56);

	return returnValue;
	else
	return input;
	end if;
end; pragma Inline (reverseDword);

end  Crypto.Symmetric.Algorithm.Blake2b_Utils;