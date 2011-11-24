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


-- Most algorithms based on Kankerson, Menezes and Vanstones
-- "Guide to Elliptic Curve Cryptograpyh" (ISBN: 0-387-95273-x)


-- f(z) = 2^m + r(z)
-- R is the binary representation of r(z)

with Crypto.Asymmetric.Prime_Tables;


separate(Crypto.Types.Big_Numbers)

package body Binfield_Utils is

    function B_Mod(Left : D_Big_Unsigned; Right : Big_Unsigned)
                  return Big_Unsigned;

    function "xor"(Left, Right: D_Big_Unsigned) return D_Big_Unsigned;

    procedure Set_Last_Index(X : in out D_Big_Unsigned);

    ---------------------------------------------------------------------------

   pragma Optimize (Time);
   use Crypto.Asymmetric.Prime_Tables;

   -- compute: a(z) + b(z) mod f(z)
   function B_Add(Left,Right : Big_Unsigned) return Big_Unsigned is
      N : constant Natural := Natural'Max(Left.Last_Index, Right.Last_Index);
      C : Big_Unsigned;
   begin
      for I  in 0..N loop
         C.Number(I) := Left.Number(i) xor Right.Number(I);
      end loop;

      for I in reverse 0..N  loop
         if C.Number(I) /= 0 then
            C.Last_Index :=I;
            exit;
         end if;
      end loop;

      return  C;
   end B_Add;

   ---------------------------------------------------------------------------


   -- compute: a(z) - b(z) mod f(z)
   -- in binary field is -a = a. so a - b = a + (-b) = a + b
   function B_Sub(Left,Right : Big_Unsigned) return Big_Unsigned is
   begin
      return B_Add(Left,Right);
   end B_Sub;


   ---------------------------------------------------------------------------

   -- compute: a(z)* z mod f(Z)
   function B_Mult(A, F : Big_Unsigned)
                  return Big_Unsigned is
      C : Big_Unsigned;
      M : constant Positive := Bit_Length(F)-1;
      N : Natural:=  M/Mod_Type'Size;
   begin
      C := Shift_Left(A,1);

      if C.Last_Index = N  then
         N:=M mod Mod_Type'Size;

         if (Shift_Right(C.Number(C.Last_Index),N)) = 1 then
            C :=  B_Add(C,F);
         end if;
      end if;
      return C;

   end B_Mult;

   ---------------------------------------------------------------------------


   --Algorithm 2.34: Right to left comb method for polynominal multiplication
   -- compute: a(z)*b(z) mod f(Z)
   function B_Mult(Left, Right, F : Big_Unsigned) return Big_Unsigned is
      C : D_Big_Unsigned;
      B : Big_Unsigned := Right;
      --      N : constant Natural := Bit_Length(F);
   begin
      for K in 0..Mod_Type'Size-1 loop
         for J in 0..Left.Last_Index loop
            if (Shift_Right(Left.Number(J),K) and 1) = 1 then
               -- add B to C{i}
               for I in J..(J+B.Last_Index) loop
                  C.Number(I) := C.Number(I) xor B.Number(I-J);
               end loop;
            end if;
         end loop;
         if K /= Mod_Type'Size-1  then
            B:=B_Mult(B,F);
         end if;
      end loop;

      Set_Last_Index(C);

      return B_Mod(C,F);

   end B_Mult;

    ---------------------------------------------------------------------------

   -- Algorithm 2.39: Polynominal squaring (with wordlength W=8)
   -- compute a(z)**2 mod f(z) on a 8 bit processor
   function B_Square8(A, F : Big_Unsigned) return Big_Unsigned is
      C : D_Big_Unsigned;
      L : Natural;
   begin
      for I in 0..A.Last_Index loop
         L := 2*I;
         C.Number(L) := Mod_Type(T8(Natural(A.Number(I) and 15)));
         L:= L+1;
         C.Number(L) :=
           Mod_Type(T8(Natural(Shift_Right(A.Number(I),4) and 15)));
      end loop;

      Set_Last_Index(C);

      return B_Mod(C,F);
   end  B_Square8;

   -------------------------------------------------------------------------

   -- Algorithm 2.39: Polynominal squaring (with word length W=n*8 for n=>0)
   -- compute a(z)**2 mod f(z)
   function B_Square(A, F : Big_Unsigned) return Big_Unsigned is
      K : constant Natural := Shift_Right(Mod_Type'Size,3);
      N : constant Natural := Shift_Right(K,1)-1;
      --M : constant Natural := Bit_Length(F);
      L : Natural;
      C : D_Big_Unsigned;
   begin
      if K = 1 then
         return B_Square8(A,F);
      else
         for I in 0..A.Last_Index loop
            L := 2*I;
            for J in reverse 0..N loop
               C.Number(L) := Shift_Left(C.Number(L),16) xor
                 Mod_Type(T16(Byte(Shift_Right(A.Number(I),8*J) and 255)));
            end loop;
            L:= L+1;
            for J  in reverse K/2..K-1 loop
               C.Number(L) := Shift_Left(C.Number(L),16) xor
                 Mod_Type(T16(Byte(Shift_Right(A.Number(I),8*J) and 255)));
            end loop;
         end loop;
      end if;

      Set_Last_Index(C);

      return B_Mod(C,F);
   end B_Square;

--------------------------------------------------------------------------

   -- It' my own secret "blow and cut" technic. ;-)
   -- compute left(z) mod right(z)
   function B_Mod(Left, Right  : Big_Unsigned) return Big_Unsigned is
      A : Natural := Bit_Length(Left);
      B : constant Natural := Bit_Length(Right);
      Result : Big_Unsigned;
   begin
      if A < B or B=0 then
        Result.Last_Index := Left.Last_Index;
        Result.Number(0..Left.Last_Index) := Left.Number(0..Left.Last_Index);
      else
         while A >= B loop
            Result  := Shift_Left(Right,A-B) xor Right;
            A := Bit_Length(Result);
         end loop;
      end if;
      return Result;
   end B_Mod;



   --------------------------------------------------------------------------

   -- Algorithm 2.49: Binary algorithm for inversion in F_{2^m}
   -- computes a(z)^{-1}
   function B_Inverse(X, F : Big_Unsigned) return Big_Unsigned is
      U : Big_Unsigned  := X;
      V : Big_Unsigned  := F;
      G1 : Big_Unsigned := Big_Unsigned_One;
      G2 : Big_Unsigned;
   begin
      if X = Big_Unsigned_Zero or F = Big_Unsigned_Zero  then
         return F;
      end if;

      while U /= Big_Unsigned_One and V /= Big_Unsigned_One loop

         while Is_Even(U) loop
            U := Shift_Right(U,1);
            if Is_Even(G1) then
               G1 := Shift_Right(G1,1);
            else
               G1 := Shift_Right(B_Add(G1,F),1);
            end if;
         end loop;

         while Is_Even(V) loop
            V := Shift_Right(V,1);
            if Is_Even(G2) then
               G2 := Shift_Right(G2,1);
            else
               G2 := Shift_Right(B_Add(G2,F),1);
            end if;
         end loop;

         if Bit_Length(U) > Bit_Length(V) then
            U  := B_Add(U,V);
            G1 := B_Add(G1,G2);
         else
            V  := B_Add(V,U);
            G2 := B_Add(G2,G1);
         end if;
      end loop;
      if U = Big_Unsigned_One then
         return  G1;
      else
         return G2;
      end if;
   end B_Inverse;

   --------------------------------------------------------------------------

   function B_Div(Left, Right, F : Big_Unsigned) return Big_Unsigned is
      R : constant Big_Unsigned := B_Inverse(Right, F);
   begin
      return B_Mult(Left,R,F);
   end B_Div;

   --------------------------------------------------------------------------
   --------------------------------------------------------------------------

   function B_Mod(Left : D_Big_Unsigned; Right : Big_Unsigned)
                 return Big_Unsigned is
      A : Natural := Bit_Length(Left);
      B : constant Natural := Bit_Length(Right);
      Result : Big_Unsigned;
   begin
      if A < B or B=0 then
         Result.Last_Index := Left.Last_Index;
         Result.Number(0..Left.Last_Index) := Left.Number(0..Left.Last_Index);
      else
         declare
            T : D_Big_Unsigned := Left;
            Z : D_Big_Unsigned;
         begin
            Z.Last_Index := Right.Last_Index;
            Z.Number(0..Right.Last_Index) := Right.Number(0..Right.Last_Index);
            while A >= B loop
               T := Shift_Left(Z,A-B) xor T;
               A := Bit_Length(T);
            end loop;
            Result.Last_Index := T.Last_Index;
            Result.Number(0..T.Last_Index) := T.Number(0..T.Last_Index);
         end;
      end if;
      return Result;
   end B_Mod;


   --------------------------------------------------------------------------

   function "xor"(Left, Right: D_Big_Unsigned) return D_Big_Unsigned  is
      Result : D_Big_Unsigned;
      M : constant Natural:= Natural'Max(Left.Last_Index, Right.Last_Index);
   begin
      for I in 0..M loop
         Result.Number(I)  := Left.Number(I) xor Right.Number(I);
      end loop;
      Set_Last_Index(Result);

      return Result;
   end "xor";


   --------------------------------------------------------------------------

   procedure Set_Last_Index(X : in out D_Big_Unsigned) is
   begin
      for I in reverse 0..D_Max_Length  loop
         if X.Number(I) /= 0 then
            X.Last_Index :=I;
            exit;
         end if;
      end loop;
   end Set_Last_Index; pragma Inline(Set_Last_Index);

end Binfield_Utils;
