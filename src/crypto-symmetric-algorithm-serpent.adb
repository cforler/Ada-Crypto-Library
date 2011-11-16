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

-------------------------------------------------------------------------------
--
-- Serpent Blockcipher
--
-- Copyright (c) 1998 Markus G. Kuhn <mkuhn@acm.org>. All rights reserved.
--
-- $Id: crypto-symmetric-algorithm-serpent.adb 1.2 Fri, 17 Dec 2004 16:55:38 +0100 shortie $
--
-------------------------------------------------------------------------------
--
-- This implementation is optimized for best execution time by use of
-- function inlining and loop unrolling. It is not intended to be used in
-- applications (such as smartcards) where machine code size matters. Best
-- compiled with highest optimization level activated and all run-time
-- checks supressed.
--
-------------------------------------------------------------------------------

package body Crypto.Symmetric.Algorithm.Serpent is

   procedure S (R : Integer; X0, X1, X2, X3 : in out Word) is
      T01, T02, T03, T04, T05, T06, T07, T08, T09,
      T10, T11, T12, T13, T14, T15, T16, T17, T18 : Word;
      W, X, Y, Z : Word :=0;
   begin
      if R = 0 then
         -- S0:   3  8 15  1 10  6  5 11 14 13  4  2  7  0  9 12
         -- depth = 5,7,4,2, Total gates=18
         T01 := X1  xor X2;
         T02 := X0  or X3;
         T03 := X0  xor X1;
         Z   := T02 xor T01;
         T05 := X2  or z;
         T06 := X0  xor X3;
         T07 := X1  or X2;
         T08 := X3  and T05;
         T09 := T03 and T07;
         Y   := T09 xor T08;
         T11 := T09 and y;
         T12 := X2  xor X3;
         T13 := T07 xor T11;
         T14 := X1  and T06;
         T15 := T06 xor T13;
         W   :=     not T15;
         T17 := W   xor T14;
         X   := T12 xor T17;
      elsif R = 1 then
         -- S1:  15 12  2  7  9  0  5 10  1 11 14  8  6 13  3  4
         -- depth = 10,7,3,5, Total gates=18
         T01 := X0  or X3;
         T02 := X2  xor X3;
         T03 :=     not X1;
         T04 := X0  xor X2;
         T05 := X0  or T03;
         T06 := X3  and T04;
         T07 := T01 and T02;
         T08 := X1  or T06;
         Y   := T02 xor T05;
         T10 := T07 xor T08;
         T11 := T01 xor T10;
         T12 := Y   xor T11;
         T13 := X1  and X3;
         Z   :=     not T10;
         X   := T13 xor T12;
         T16 := T10 or x;
         T17 := T05 and T16;
         W   := X2  xor T17;
      elsif R = 2 then
         -- S2:   8  6  7  9  3 12 10 15 13  1 14  4  0 11  5  2
         -- depth = 3,8,11,7, Total gates=16
         T01 := X0  or X2;
         T02 := X0  xor X1;
         T03 := X3  xor T01;
         W   := T02 xor T03;
         T05 := X2  xor w;
         T06 := X1  xor T05;
         T07 := X1  or T05;
         T08 := T01 and T06;
         T09 := T03 xor T07;
         T10 := T02 or T09;
         X   := T10 xor T08;
         T12 := X0  or X3;
         T13 := T09 xor x;
         T14 := X1  xor T13;
         Z   :=     not T09;
         Y   := T12 xor T14;
      elsif R = 3 then
         -- S3:   0 15 11  8 12  9  6  3 13  1  2  4 10  7  5 14
         -- depth = 8,3,5,5, Total gates=18
         T01 := X0  xor X2;
         T02 := X0  or X3;
         T03 := X0  and X3;
         T04 := T01 and T02;
         T05 := X1  or T03;
         T06 := X0  and X1;
         T07 := X3  xor T04;
         T08 := X2  or T06;
         T09 := X1  xor T07;
         T10 := X3  and T05;
         T11 := T02 xor T10;
         Z   := T08 xor T09;
         T13 := X3  or z;
         T14 := X0  or T07;
         T15 := X1  and T13;
         Y   := T08 xor T11;
         W   := T14 xor T15;
         X   := T05 xor T04;
      elsif R = 4 then
         -- S4:   1 15  8  3 12  0 11  6  2  5  4 10  9 14  7 13
         -- depth = 6,7,5,3, Total gates=19
         T01 := X0  or X1;
         T02 := X1  or X2;
         T03 := X0  xor T02;
         T04 := X1  xor X3;
         T05 := X3  or T03;
         T06 := X3  and T01;
         Z   := T03 xor T06;
         T08 := Z   and T04;
         T09 := T04 and T05;
         T10 := X2  xor T06;
         T11 := X1  and X2;
         T12 := T04 xor T08;
         T13 := T11 or T03;
         T14 := T10 xor T09;
         T15 := X0  and T05;
         T16 := T11 or T12;
         Y   := T13 xor T08;
         X   := T15 xor T16;
         W   :=     not T14;
      elsif R = 5 then
         -- S5:  15  5  2 11  4 10  9 12  0  3 14  8 13  6  7  1
         -- depth = 4,6,8,6, Total gates=17
         T01 := X1  xor X3;
         T02 := X1  or X3;
         T03 := X0  and T01;
         T04 := X2  xor T02;
         T05 := T03 xor T04;
         W   :=     not T05;
         T07 := X0  xor T01;
         T08 := X3  or w;
         T09 := X1  or T05;
         T10 := X3  xor T08;
         T11 := X1  or T07;
         T12 := T03 or w;
         T13 := T07 or T10;
         T14 := T01 xor T11;
         Y   := T09 xor T13;
         X   := T07 xor T08;
         Z   := T12 xor T14;
      elsif R = 6 then
         -- S6:   7  2 12  5  8  4  6 11 14  9  1 15 13  3 10  0
         -- depth = 8,3,6,3, Total gates=19
         T01 := X0  and X3;
         T02 := X1  xor X2;
         T03 := X0  xor X3;
         T04 := T01 xor T02;
         T05 := X1  or X2;
         X   :=     not T04;
         T07 := T03 and T05;
         T08 := X1  and x;
         T09 := X0  or X2;
         T10 := T07 xor T08;
         T11 := X1  or X3;
         T12 := X2  xor T11;
         T13 := T09 xor T10;
         Y   :=     not T13;
         T15 := X   and T03;
         Z   := T12 xor T07;
         T17 := X0  xor X1;
         T18 := Y   xor T15;
         W   := T17 xor T18;
      elsif R = 7 then
         -- S7:   1 13 15  0 14  8  2 11  7  4 12 10  9  3  5  6
         -- depth = 10,7,10,4, Total gates=19
         T01 := X0  and X2;
         T02 :=     not X3;
         T03 := X0  and T02;
         T04 := X1  or T01;
         T05 := X0  and X1;
         T06 := X2  xor T04;
         Z   := T03 xor T06;
         T08 := X2  or z;
         T09 := X3  or T05;
         T10 := X0  xor T08;
         T11 := T04 and z;
         X   := T09 xor T10;
         T13 := X1  xor x;
         T14 := T01 xor x;
         T15 := X2  xor T05;
         T16 := T11 or T13;
         T17 := T02 or T14;
         W   := T15 xor T17;
         Y   := X0  xor T16;
      end if;
      X0 := W;
      X1 := X;
      X2 := Y;
      X3 := Z;
   end S;


   -- Inverse Sbox function

   procedure SI (R : Integer; X0, X1, X2, X3 : in out Word) is
      T01, T02, T03, T04, T05, T06, T07, T08, T09,
      T10, T11, T12, T13, T14, T15, T16, T17, T18 : Word;
      W, X, Y, Z : Word := 0;
   begin
      if R = 0 then
         -- InvS0:  13  3 11  0 10  6  5 12  1 14  4  7 15  9  8  2
         -- depth = 8,4,3,6, Total gates=19
         T01 := X2  xor X3;
         T02 := X0  or X1;
         T03 := X1  or X2;
         T04 := X2  and T01;
         T05 := T02 xor T01;
         T06 := X0  or T04;
         Y   :=     not T05;
         T08 := X1  xor X3;
         T09 := T03 and T08;
         T10 := X3  or y;
         X   := T09 xor T06;
         T12 := X0  or T05;
         T13 := X   xor T12;
         T14 := T03 xor T10;
         T15 := X0  xor X2;
         Z   := T14 xor T13;
         T17 := T05 and T13;
         T18 := T14 or T17;
         W   := T15 xor T18;
      elsif R = 1 then
         -- InvS1:   5  8  2 14 15  6 12  3 11  4  7  9  1 13 10  0
         -- depth = 7,4,5,3, Total gates=18
         T01 := X0  xor X1;
         T02 := X1  or X3;
         T03 := X0  and X2;
         T04 := X2  xor T02;
         T05 := X0  or T04;
         T06 := T01 and T05;
         T07 := X3  or T03;
         T08 := X1  xor T06;
         T09 := T07 xor T06;
         T10 := T04 or T03;
         T11 := X3  and T08;
         Y   :=     not T09;
         X   := T10 xor T11;
         T14 := X0  or y;
         T15 := T06 xor x;
         Z   := T01 xor T04;
         T17 := X2  xor T15;
         W   := T14 xor T17;
      elsif R = 2 then
         -- InvS2:  12  9 15  4 11 14  1  2  0  3  6 13  5  8 10  7
         -- depth = 3,6,8,3, Total gates=18
         T01 := X0  xor X3;
         T02 := X2  xor X3;
         T03 := X0  and X2;
         T04 := X1  or T02;
         W   := T01 xor T04;
         T06 := X0  or X2;
         T07 := X3  or w;
         T08 :=     not X3;
         T09 := X1  and T06;
         T10 := T08 or T03;
         T11 := X1  and T07;
         T12 := T06 and T02;
         Z   := T09 xor T10;
         X   := T12 xor T11;
         T15 := X2  and z;
         T16 := W   xor x;
         T17 := T10 xor T15;
         Y   := T16 xor T17;
      elsif R = 3 then
         -- InvS3:   0  9 10  7 11 14  6 13  3  5 12  2  4  8 15  1
         -- depth = 3,6,4,4, Total gates=17
         T01 := X2  or X3;
         T02 := X0  or X3;
         T03 := X2  xor T02;
         T04 := X1  xor T02;
         T05 := X0  xor X3;
         T06 := T04 and T03;
         T07 := X1  and T01;
         Y   := T05 xor T06;
         T09 := X0  xor T03;
         W   := T07 xor T03;
         T11 := W   or T05;
         T12 := T09 and T11;
         T13 := X0  and y;
         T14 := T01 xor T05;
         X   := X1  xor T12;
         T16 := X1  or T13;
         Z   := T14 xor T16;
      elsif R = 4 then
         -- InvS4:   5  0  8  3 10  9  7 14  2 12 11  6  4 15 13  1
         -- depth = 6,4,7,3, Total gates=17
         T01 := X1  or X3;
         T02 := X2  or X3;
         T03 := X0  and T01;
         T04 := X1  xor T02;
         T05 := X2  xor X3;
         T06 :=     not T03;
         T07 := X0  and T04;
         X   := T05 xor T07;
         T09 := X   or T06;
         T10 := X0  xor T07;
         T11 := T01 xor T09;
         T12 := X3  xor T04;
         T13 := X2  or T10;
         Z   := T03 xor T12;
         T15 := X0  xor T04;
         Y   := T11 xor T13;
         W   := T15 xor T09;
      elsif R = 5 then
         -- InvS5:   8 15  2  9  4  1 13 14 11  6  5  3  7 12 10  0
         -- depth = 4,6,9,7, Total gates=17
         T01 := X0  and X3;
         T02 := X2  xor T01;
         T03 := X0  xor X3;
         T04 := X1  and T02;
         T05 := X0  and X2;
         W   := T03 xor T04;
         T07 := X0  and w;
         T08 := T01 xor w;
         T09 := X1  or T05;
         T10 :=     not X1;
         X   := T08 xor T09;
         T12 := T10 or T07;
         T13 := W   or x;
         Z   := T02 xor T12;
         T15 := T02 xor T13;
         T16 := X1  xor X3;
         Y   := T16 xor T15;
      elsif R = 6 then
         -- InvS6:  15 10  1 13  5  3  6  0  4  9 14  7  2 12  8 11
         -- depth = 5,3,8,6, Total gates=19
         T01 := X0  xor X2;
         T02 :=     not X2;
         T03 := X1  and T01;
         T04 := X1  or T02;
         T05 := X3  or T03;
         T06 := X1  xor X3;
         T07 := X0  and T04;
         T08 := X0  or T02;
         T09 := T07 xor T05;
         X   := T06 xor T08;
         W   :=     not T09;
         T12 := X1  and w;
         T13 := T01 and T05;
         T14 := T01 xor T12;
         T15 := T07 xor T13;
         T16 := X3  or T02;
         T17 := X0  xor x;
         Z   := T17 xor T15;
         Y   := T16 xor T14;
      elsif R = 7 then
         -- InvS7:   3  0  6 13  9 14 15  8  5 12 11  7 10  1  4  2
         -- depth := 9,7,3,3, Total gates:=18
         T01 := X0  and X1;
         T02 := X0  or X1;
         T03 := X2  or T01;
         T04 := X3  and T02;
         Z   := T03 xor T04;
         T06 := X1  xor T04;
         T07 := X3  xor z;
         T08 :=     not T07;
         T09 := T06 or T08;
         T10 := X1  xor X3;
         T11 := X0  or X3;
         X   := X0  xor T09;
         T13 := X2  xor T06;
         T14 := X2  and T11;
         T15 := X3  or x;
         T16 := T01 or T10;
         W   := T13 xor T15;
         Y   := T14 xor T16;
      end if;
      X0 := W;
      X1 := X;
      X2 := Y;
      X3 := Z;
   end SI;


   -- Linear Transform

   procedure Tr (X0, X1, X2, X3 : in out Word) is
   begin
      X0 := Rotate_Left(X0, 13);
      X2 := Rotate_Left(X2, 3);
      X1 := X1 xor X0 xor X2;
      X3 := X3 xor X2 xor Shift_Left(X0, 3);
      X1 := Rotate_Left(X1, 1);
      X3 := Rotate_Left(X3, 7);
      X0 := X0 xor X1 xor X3;
      X2 := X2 xor X3 xor Shift_Left(X1, 7);
      X0 := Rotate_Left(X0, 5);
      X2 := Rotate_Left(X2, 22);
   end Tr;


   -- Inverse Linear Transform

   procedure TrI (X0, X1, X2, X3 : in out Word) is
   begin
      X2 := Rotate_Right(X2, 22);
      X0 := Rotate_Right(X0, 5);
      X2 := X2 xor X3 xor Shift_Left(X1, 7);
      X0 := X0 xor X1 xor X3;
      X3 := Rotate_Right(X3, 7);
      X1 := Rotate_Right(X1, 1);
      X3 := X3 xor X2 xor Shift_Left(X0, 3);
      X1 := X1 xor X0 xor X2;
      X2 := Rotate_Right(X2, 3);
      X0 := Rotate_Right(X0, 13);
   end TrI;


   procedure Keying (W : Roundkey_Serpent256;
                     R : Integer;
                     X0, X1, X2, X3 : in out Word) is
   begin
      X0 := X0 xor W(4*R);
      X1 := X1 xor W(4*R+1);
      X2 := X2 xor W(4*R+2);
      X3 := X3 xor W(4*R+3);
   end Keying;


   pragma Inline(S, SI, Tr, TrI, Keying);


   procedure Prepare_Key (K : in B_Block256; W : out Roundkey_Serpent256) is
   begin
      for I in 0..7 loop
         W(-8+I) := R_To_Word(Bytes(K(4*I .. 4*I+3)));
      end loop;
      for I in 0..131 loop
         W(I) := Rotate_Left(W(I-8) xor W(I-5) xor
                             W(I-3) xor W(I-1) xor
                             16#9e3779b9# xor Word(I), 11);
      end loop;
      S(3, W(  0), W(  1), W(  2), W(  3));
      S(2, W(  4), W(  5), W(  6), W(  7));
      S(1, W(  8), W(  9), W( 10), W( 11));
      S(0, W( 12), W( 13), W( 14), W( 15));
      S(7, W( 16), W( 17), W( 18), W( 19));
      S(6, W( 20), W( 21), W( 22), W( 23));
      S(5, W( 24), W( 25), W( 26), W( 27));
      S(4, W( 28), W( 29), W( 30), W( 31));
      S(3, W( 32), W( 33), W( 34), W( 35));
      S(2, W( 36), W( 37), W( 38), W( 39));
      S(1, W( 40), W( 41), W( 42), W( 43));
      S(0, W( 44), W( 45), W( 46), W( 47));
      S(7, W( 48), W( 49), W( 50), W( 51));
      S(6, W( 52), W( 53), W( 54), W( 55));
      S(5, W( 56), W( 57), W( 58), W( 59));
      S(4, W( 60), W( 61), W( 62), W( 63));
      S(3, W( 64), W( 65), W( 66), W( 67));
      S(2, W( 68), W( 69), W( 70), W( 71));
      S(1, W( 72), W( 73), W( 74), W( 75));
      S(0, W( 76), W( 77), W( 78), W( 79));
      S(7, W( 80), W( 81), W( 82), W( 83));
      S(6, W( 84), W( 85), W( 86), W( 87));
      S(5, W( 88), W( 89), W( 90), W( 91));
      S(4, W( 92), W( 93), W( 94), W( 95));
      S(3, W( 96), W( 97), W( 98), W( 99));
      S(2, W(100), W(101), W(102), W(103));
      S(1, W(104), W(105), W(106), W(107));
      S(0, W(108), W(109), W(110), W(111));
      S(7, W(112), W(113), W(114), W(115));
      S(6, W(116), W(117), W(118), W(119));
      S(5, W(120), W(121), W(122), W(123));
      S(4, W(124), W(125), W(126), W(127));
      S(3, W(128), W(129), W(130), W(131));
   end Prepare_Key;


   function Encrypt (W : in Roundkey_Serpent256; Plaintext : in B_Block128)
                    return B_Block128 is
      Ciphertext : Bytes(B_Block128'Range);
      X0, X1, X2, X3 : Word;
   begin
      X0 := R_To_Word(Bytes(Plaintext( 0 ..  3)));
      X1 := R_To_Word(Bytes(Plaintext( 4 ..  7)));
      X2 := R_To_Word(Bytes(Plaintext( 8 .. 11)));
      X3 := R_To_Word(Bytes(Plaintext(12 .. 15)));

      Keying(W,  0, X0, X1, X2, X3); S(0, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  1, X0, X1, X2, X3); S(1, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  2, X0, X1, X2, X3); S(2, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  3, X0, X1, X2, X3); S(3, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  4, X0, X1, X2, X3); S(4, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  5, X0, X1, X2, X3); S(5, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  6, X0, X1, X2, X3); S(6, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  7, X0, X1, X2, X3); S(7, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  8, X0, X1, X2, X3); S(0, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W,  9, X0, X1, X2, X3); S(1, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 10, X0, X1, X2, X3); S(2, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 11, X0, X1, X2, X3); S(3, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 12, X0, X1, X2, X3); S(4, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 13, X0, X1, X2, X3); S(5, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 14, X0, X1, X2, X3); S(6, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 15, X0, X1, X2, X3); S(7, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 16, X0, X1, X2, X3); S(0, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 17, X0, X1, X2, X3); S(1, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 18, X0, X1, X2, X3); S(2, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 19, X0, X1, X2, X3); S(3, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 20, X0, X1, X2, X3); S(4, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 21, X0, X1, X2, X3); S(5, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 22, X0, X1, X2, X3); S(6, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 23, X0, X1, X2, X3); S(7, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 24, X0, X1, X2, X3); S(0, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 25, X0, X1, X2, X3); S(1, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 26, X0, X1, X2, X3); S(2, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 27, X0, X1, X2, X3); S(3, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 28, X0, X1, X2, X3); S(4, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 29, X0, X1, X2, X3); S(5, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 30, X0, X1, X2, X3); S(6, X0, X1, X2, X3); Tr(X0, X1, X2, X3);
      Keying(W, 31, X0, X1, X2, X3);
      S(7, X0, X1, X2, X3);
      Keying(W, 32, X0, X1, X2, X3);

      Ciphertext( 0 ..  3) := R_To_Bytes(X0);
      Ciphertext( 4 ..  7) := R_To_Bytes(X1);
      Ciphertext( 8 .. 11) := R_To_Bytes(X2);
      Ciphertext(12 .. 15) := R_To_Bytes(X3);

      return B_Block128(Ciphertext);
   end Encrypt;


   function Decrypt (W : in Roundkey_Serpent256; Ciphertext :  in B_Block128)
                    return B_Block128 is
      Plaintext  : Bytes(B_Block128'Range);
      X0, X1, X2, X3 : Word;
   begin
      X0 := R_To_Word(Bytes(Ciphertext( 0 ..  3)));
      X1 := R_To_Word(Bytes(Ciphertext( 4 ..  7)));
      X2 := R_To_Word(Bytes(Ciphertext( 8 .. 11)));
      X3 := R_To_Word(Bytes(Ciphertext(12 .. 15)));

      Keying(W, 32, X0, X1, X2, X3);
      SI(7, X0, X1, X2, X3);
      Keying(W, 31, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(6, X0, X1, X2, X3); Keying(W,30, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(5, X0, X1, X2, X3); Keying(W,29, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(4, X0, X1, X2, X3); Keying(W,28, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(3, X0, X1, X2, X3); Keying(W,27, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(2, X0, X1, X2, X3); Keying(W,26, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(1, X0, X1, X2, X3); Keying(W,25, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(0, X0, X1, X2, X3); Keying(W,24, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(7, X0, X1, X2, X3); Keying(W,23, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(6, X0, X1, X2, X3); Keying(W,22, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(5, X0, X1, X2, X3); Keying(W,21, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(4, X0, X1, X2, X3); Keying(W,20, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(3, X0, X1, X2, X3); Keying(W,19, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(2, X0, X1, X2, X3); Keying(W,18, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(1, X0, X1, X2, X3); Keying(W,17, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(0, X0, X1, X2, X3); Keying(W,16, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(7, X0, X1, X2, X3); Keying(W,15, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(6, X0, X1, X2, X3); Keying(W,14, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(5, X0, X1, X2, X3); Keying(W,13, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(4, X0, X1, X2, X3); Keying(W,12, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(3, X0, X1, X2, X3); Keying(W,11, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(2, X0, X1, X2, X3); Keying(W,10, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(1, X0, X1, X2, X3); Keying(W, 9, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(0, X0, X1, X2, X3); Keying(W, 8, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(7, X0, X1, X2, X3); Keying(W, 7, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(6, X0, X1, X2, X3); Keying(W, 6, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(5, X0, X1, X2, X3); Keying(W, 5, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(4, X0, X1, X2, X3); Keying(W, 4, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(3, X0, X1, X2, X3); Keying(W, 3, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(2, X0, X1, X2, X3); Keying(W, 2, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(1, X0, X1, X2, X3); Keying(W, 1, X0, X1, X2, X3);
      TrI(X0, X1, X2, X3); SI(0, X0, X1, X2, X3); Keying(W, 0, X0, X1, X2, X3);

      Plaintext( 0 ..  3) := R_To_Bytes(X0);
      Plaintext( 4 ..  7) := R_To_Bytes(X1);
      Plaintext( 8 .. 11) := R_To_Bytes(X2);
      Plaintext(12 .. 15) := R_To_Bytes(X3);

      return B_Block128(Plaintext);
   end Decrypt;

   ---------------------------------------------------------------------------

   procedure Prepare_Key256(Key       : in B_Block256;
                            Cipherkey : out Cipherkey_Serpent256) is
   begin
      Prepare_Key(Key, Roundkey_Serpent256(Cipherkey));
   end Prepare_Key256;

   ---------------------------------------------------------------------------

   procedure Encrypt256(Cipherkey  : in  Cipherkey_Serpent256;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Ciphertext:= Encrypt(Roundkey_Serpent256(Cipherkey), Plaintext);
   end Encrypt256;

   ---------------------------------------------------------------------------

   procedure Decrypt256(Cipherkey  : in  Cipherkey_Serpent256;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Plaintext :=  Decrypt(Roundkey_Serpent256(Cipherkey), Ciphertext);
   end Decrypt256;

end Crypto.Symmetric.Algorithm.Serpent;
