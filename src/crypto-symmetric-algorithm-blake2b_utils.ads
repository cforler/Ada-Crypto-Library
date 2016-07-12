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

package Crypto.Symmetric.Algorithm.Blake2b_Utils is

blake2bIV : Dwords(0..7) := (	16#08c9bcf367e6096a#, 16#3ba7ca8485ae67bb#,
								16#2bf894fe72f36e3c#, 16#f1361d5f3af54fa5#,
								16#d182e6ad7f520e51#, 16#1f6c3e2b8c68059b#,
								16#6bbd41fbabd9831f#, 16#79217e1319cde05b#);

subtype B2HashType is Dwords(0..7);

type TwoDInteger is
    array (Natural range <>, Natural range <>) of Integer;

blake2bSigma : TwoDInteger(0..11, 0..15) :=
((  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ) ,
  ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ) ,
  ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ) ,
  (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ) ,
  (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ) ,
  (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ) ,
  ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ) ,
  ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ) ,
  (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ) ,
  ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ) ,
  (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ) ,
  ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ));

type VectorInt is array (Integer range <>) of Integer;

	BLAKE2B_SALTBYTES : Integer  := 16;
	BLAKE2B_OUTBYTES : Integer := 64;
    BLAKE2B_PERSONALBYTES : Integer := 16;
	BLAKE2B_BLOCKBYTES : Integer := 128;

type Blake2bParam is
    record
       digest_length 	: Integer;
       key_length		: Integer;
       fanout			: Integer;
       depth			: Integer;
       leaf_length		: Integer;
       node_offset		: Integer;
       node_depth		: Integer;
       inner_length		: Integer;
       reserved			: Bytes(0..13);
       salt				: Bytes(0..BLAKE2B_SALTBYTES-1);
       personal			: Bytes(0..BLAKE2B_PERSONALBYTES-1);
    end record;

type Blake2bState is
    record
    	h 		: B2HashType;
    	t 		: DWords(0..1);
    	f 		: DWords(0..1);
    	buf 	: Bytes(0..2*BLAKE2B_BLOCKBYTES -1);
    	buflen	: Natural;
    	lastNode: Byte;
    end record;

function fblamka(x : in DWord;
				 y : in Dword) return Dword;

procedure Blake2bInit(	S 		: in out Blake2bState;
						outlen	: in Integer);

procedure Blake2bInitParam	(	S 		: in out Blake2bState;
								p	: in Blake2bParam);

procedure blake2bIncrementCounter(	S 		: in out Blake2bState;
								inc	: in Natural);

function reverseDword(input : in Dword) return Dword;

private

   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.Blake2b_Utils;
