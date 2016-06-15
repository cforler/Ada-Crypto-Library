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


package body Crypto.Symmetric.Algorithm.Blake2b1 is

procedure resetState(Ctx : in out Blake2b_Context) is
begin
  Blake2bInit(Ctx.Context, 64);
end resetState;

procedure Compress( Ctx : in out Blake2b_Context;
                    Number : in Integer;
                    i1    : in DW_Block512;
                    i2    : in DW_Block512;
                    Output: out DW_Block512) is
function Cast is new Ada.Unchecked_Conversion (B2Hashtype, DW_Block512);

begin
  -- innerCtx.buf(0..127) := Bytes(BSa);

  Ctx.Context.buf(0..63) := to_Bytes(i1);
  Ctx.Context.buf(64..127) := to_Bytes(i2);

  Ctx.Context.buflen := 128;
  blake2bIncrementCounter(Ctx.Context, Ctx.Context.buflen);
  Ctx.Context.f(0):= (16#FFFFFFFFFFFFFFFF#);
  
  blake2bCompress(Number mod 12, Ctx.Context, Ctx.Context.buf);

  Output := Cast(Ctx.Context.h);


end Compress;



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

end G;


procedure blake2bCompress(Number : in Integer;
                          S       : in out Blake2bState;
              block     : in Bytes) is
  m : DWords(0..15);
  v : DWords(0..15);
begin
  
  --  consider load() function
  m := to_Dwords(block(0..127));
  v(0..7) := s.h;
  v(8) := blake2bIV(0);
  v(9) := blake2bIV(1);
  v(10) := blake2bIV(2);
  v(11) := blake2bIV(3);
  v(12) := S.t(0) xor blake2bIV(4);
  v(13) := S.t(1) xor blake2bIV(5);
  v(14) := S.f(0) xor blake2bIV(6);
  v(15) := S.f(1) xor blake2bIV(7);
  
  for i in Natural range 0..15 loop
    v(i):= reverseDword(v(i));
    m(i) := reverseDword(m(i));
  end loop;

  Round_Blake2(Number, v, m);


  for i in Natural range 0..15 loop
    v(i):= reverseDword(v(i));
  end loop;

  S.h := S.h xor v(0..7) xor v(8..15);
  
  null;

end;


procedure Round_Blake2(r: in Natural;
            v: in out Dwords;
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
end Round_Blake2;

end  Crypto.Symmetric.Algorithm.Blake2b1;





