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

with Crypto.Random;
with Crypto.Asymmetric.Prime_Tables;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;

separate(Crypto.Types.Big_Numbers)

package body Mod_Utils is

   pragma Optimize (Time);
   use Crypto.Asymmetric.Prime_Tables;


   ---------------------------------------------------------------------------

   function Patch(Item, N : Big_Unsigned)
                 return Big_Unsigned is
      Diff : constant Big_Unsigned:=((Big_Unsigned_Last - N) + 1) mod N;
   begin
      return Add(Item,Diff,N);
   end Patch; pragma Inline(Patch);

   ---------------------------------------------------------------------------

   function Add(Left, Right, N : Big_Unsigned) return Big_Unsigned is
      L : constant Big_Unsigned := Left mod N;
      R : constant Big_Unsigned := Right mod N;
      Result : constant Big_Unsigned := L + R;
   begin
      if Result < Max(L,R) then
         return Patch(Result,N);
      else return
        Result mod N;
      end if;
   end Add;

   ---------------------------------------------------------------------------

   function Sub(Left, Right, N : Big_Unsigned) return Big_Unsigned is
      L : constant Big_Unsigned := Left  mod N;
      R : constant Big_Unsigned := Right mod N;
   begin
      if R > L then
         return N - R + L;
      else return L-R;
      end if;
   end Sub;

   ---------------------------------------------------------------------------

   function Div(Left, Right, N : Big_Unsigned) return Big_Unsigned is
   begin
      return Mult(Left,Inverse(Right,N),N);
   end Div; pragma Inline(Div);


   ---------------------------------------------------------------------------

   --from Erik-Zenners handout "Zahlentheoretische Algorithmen"
   function Pow(Base, Exponent, N : Big_Unsigned) return Big_Unsigned is
      L : constant Big_Unsigned := Base mod N;
      R : constant Big_Unsigned := Exponent;
      Result : Big_Unsigned := Big_Unsigned_One;
   begin
      if L = Big_Unsigned_Zero or L = Big_Unsigned_One then
         return L;
      elsif R = Big_Unsigned_Zero then return Big_Unsigned_One;
      else
         -- Square_And_Muliply
         for I in reverse  0..Bit_Length(R)-1 loop
            Result :=  Mult(Result,Result,N);
            if (Shift_Right(R, I) mod 2) = Big_Unsigned_One then
               Result := Mult(Result,L,N);
            end if;
         end loop;
         return Result mod N;
      end if;
   end Pow;

   ---------------------------------------------------------------------------

   --based on  Erik-Zenners handout "Zahlentheoretische Algorithmen"
   -- (ext. Euklid)
   -- This function returns Big_unsigned_Zero if X have no inverse mod n
   function Inverse(X, N : Big_Unsigned) return Big_Unsigned is
      B : Big_Unsigned := X mod N;
      A : Big_Unsigned := N;
   begin
      -- if gcd(A,B) /= 1 then A have no inverse mod B
      if B = Big_Unsigned_Zero or A = Big_Unsigned_Zero or
        Gcd(A,B) /= Big_Unsigned_One then
         return  Big_Unsigned_Zero;
      end if;

      declare
         T : Big_Unsigned := Big_Unsigned_One;
         Tstrich,  Tempt : Big_Unsigned;
         Q, R : Big_Unsigned;
      begin
         loop
            Big_Div(A,B,Q,R);
            if(R = Big_Unsigned_Zero) then
               return T;
            end if;

            A:=B;
            B:=R;

            Tempt:=T;

            T:=Sub(Tstrich,Mult(Q,T,N),N);

            Tstrich:=Tempt;
         end loop;
      end;
   end Inverse;

   ---------------------------------------------------------------------------

   function Get_Random(N : Big_Unsigned) return Big_Unsigned is
      Result : Big_Unsigned;
      K : constant:= 3; -- Byte'Size = 8 = 2^N = 2^3
      B : Byte;
   begin
      for I in 0..N.Last_Index loop
         for J in 0..(Shift_Right(Mod_Type(Mod_Type'Size),K)-1) loop
            Crypto.Random.Read(B);
            Result.Number(I) := Result.Number(I) or
              Shift_Left(Mod_Type(B),Natural(Shift_Left(J,K)));
         end loop;
      end loop;

      for I in reverse 0..N.Last_Index loop
         if Result.Number(I) /= 0 then
            Result.Last_Index := I;
            exit;
         end if;
      end loop;
      return Result mod N ;
   end Get_Random;

   ---------------------------------------------------------------------------

   -- this function returns true if X is a Mersenne prim number
   function Lucas_Lehmer_Test(X : Big_Unsigned) return Boolean is
      Is_Mp : Boolean := false;
   begin

      if X.Last_Index = 0 then
         for I in 2..Mod_Type'Size-1 loop
            if  X.Number(0) = Shift_Left(2,I)-1 then
               Is_Mp := True;
               exit;
            end if;
         end loop;
         if Is_Mp = False then return False;
         end if;
      else
         for I in 0..X.Last_Index loop
            if X.Number(I) /= Mod_Type'Last then return False;
            end if;
         end loop;
      end if;

      declare
         P : constant Mod_Type := Mod_Type(Bit_Length(X)-1);
         S : Big_Unsigned := Big_Unsigned_Two+2; --S(1) = 4;
      begin
         for I in 2..P-1 loop
            S := (Mult(S,S,X) - 2) mod X;
         end loop;

         if S = Big_Unsigned_Zero then return True;
         else return False;
         end if;
      end;
   end Lucas_Lehmer_Test;

   ---------------------------------------------------------------------------

   --from Erik-Zenners handout "Zahlentheoretische Algorithmen"
   function Is_Miller_Rabin_Witness(Wit, X : Big_Unsigned) return Boolean is

      B : constant Big_Unsigned := X-1;
      Result : Big_Unsigned := Big_Unsigned_One;
      Root : Big_Unsigned;
   begin
      for I in reverse 0..Bit_Length(B)-1 loop
         Root := Result;
         Result := Mult(Result, Result, X);
         if ((Result = Big_Unsigned_One) and
             (Root /= Big_Unsigned_One and Root /= B)) then return True;
         elsif (Shift_Right(B,I) mod 2) = Big_Unsigned_One then
            Result := Mult(Result, Wit, X);
         end if;
      end loop;
      if Result /= Big_Unsigned_One then return True;
      else return False;
      end if;
   end Is_Miller_Rabin_Witness;

   ---------------------------------------------------------------------------

   -- Test if Wit is a witness for N
   -- If Wit is a wittness then N is no prime
    function Is_Simple_Witness(Wit, N : Big_Unsigned) return Boolean is
    begin
       -- is Wit a "Miller-Rabin"-witness
       if (Wit /= (N-Big_Unsigned_One)) and  (Wit /= Big_Unsigned_One) and
         Mult(Wit,Wit,N) = Big_Unsigned_One  then return True;

       elsif Gcd(Wit,N) /= Big_Unsigned_One then  return True;

       -- is Wit a "Fermat-Witness"
       -- elsif Pow(Wit,N-1,N) /= Big_Unsigned_One then  return True;
       else return False;
       end if;
    end Is_Simple_Witness;

    ---------------------------------------------------------------------------


     -- Returns true if N passes the specified number of Miller-Rabin tests.
   function Passed_Miller_Rabin_Test(X : Big_Unsigned; S : Positive)
                                    return Boolean is
      Witness : Big_Unsigned;
   begin
      -- Do the tests
      for I in 1..S loop
         -- Generate a uniform random on (1, X)
         loop
            Witness := Get_Random(X);
            exit when Witness > Big_Unsigned_One;
         end loop;
         if Is_Miller_Rabin_Witness(Witness, X) then
            return False;
         end if;
         end loop;
      return true;
   end Passed_Miller_Rabin_Test;

   ---------------------------------------------------------------------------

   function Pass_Prime_Test(X : Big_Unsigned; Status : Hardness)
                           return Boolean is
      Rounds : Natural;
      X_Bit_Size : constant Natural := Bit_Length(X);
   begin
       if X < Big_Unsigned_Two then return False;
       elsif Is_Even(X) then
          if X = Big_Unsigned_Two then return True;
          else return False;
          end if;
       end if;

       --X is odd

       for I in One_Digit_Primes'First+1..One_Digit_Primes'Last loop
         if X = Mod_Type(One_Digit_Primes(I)) then return true;
         elsif X mod Mod_Type(One_Digit_Primes(I)) = Big_Unsigned_Zero then
            return False;
         end if;
       end loop;

      for I in Two_Digit_Primes'Range loop
         if X = Mod_Type(Two_Digit_Primes(I)) then return true;
         elsif X mod Mod_Type(Two_Digit_Primes(I)) = Big_Unsigned_Zero then
            return False;
         end if;
      end loop;

      if Lucas_Lehmer_Test(X) then
         return True;
      end if;

      for I in Three_Digit_Primes'Range loop
         if X = Mod_Type(Three_Digit_Primes(I)) then return true;
         elsif X mod Mod_Type(Three_Digit_Primes(I)) = Big_Unsigned_Zero then
            return False;
         end if;
      end loop;

       -- The relationship between the certainty and the number of rounds
      -- we perform is given in the draft standard ANSI X9.80, "PRIME
      -- NUMBER GENERATION, PRIMALITY TESTING, AND PRIMALITY CERTIFICATES".
      -- Comment:
      -- I don't have a look on this paper. =:) I borrowed this
      -- "algorithmen" from the j2sdk1.4.1 library (java/math/BigInteger.java)
      -- If you have the permission to send me the draft standard ANSI X9.80
      -- then send it, please!
      -- I'm a student. I have no money for ANSI or IEEE drafts. :-(
      -- It's right to require money to read a draft?
      -- This really really sucks! SCNR!

      if    (X_Bit_Size <  100) then Rounds := 50;
      elsif (X_Bit_Size <  256) then Rounds := 27;
      elsif (X_Bit_Size <  512) then Rounds := 15;
      elsif (X_Bit_Size <  768) then Rounds :=  8;
      elsif (X_Bit_Size < 1024) then Rounds :=  4;
      else                           Rounds :=  2;
      end if;

      declare
         Witness : Big_Unsigned;
      begin
         if Status = Weak then
            for I in 1..Rounds loop
               loop
                  Witness := Get_Random(X);
                  exit when Witness > Big_Unsigned_Two;
               end loop;
               if Is_Simple_Witness(Witness,X) then return False;
               end if;
            end loop;
         else
            for I in 1..Rounds loop
               loop
                  Witness := Get_Random(X);
                  exit when Witness > Big_Unsigned_Two;
               end loop;
               if Is_Miller_Rabin_Witness(Witness,X) then return False;
               end if;
            end loop;
         end if;
      end;
      return True;
    end Pass_Prime_Test;

    ---------------------------------------------------------------------------


    function Is_Prime(X : Big_Unsigned) return Boolean is
    begin
       return Pass_Prime_Test(X, Strong);
    end Is_Prime; pragma Inline (Is_Prime);

    ---------------------------------------------------------------------------

    -- This function is faster then Is_prime but a lot of no strong pseudo
    -- primes pass this test
    function Looks_Like_A_Prime(X : Big_Unsigned) return Boolean is
    begin
      return Pass_Prime_Test(X, Weak);
    end Looks_Like_A_Prime; pragma Inline(Looks_Like_A_Prime);

   ---------------------------------------------------------------------------

    function Get_Prime(N : Big_Unsigned) return Big_Unsigned is
       Result : Big_Unsigned := Get_Random(N);
   begin
      if N <= Big_Unsigned_Two then
         raise Constraint_Error;
      end if;

      -- make sure that Result is odd
      Result.Number(0) := Result.Number(0) or 1;
      loop
         if Is_Prime(Result) then  return Result;
         else  Result := (Result+2) mod N ;
         end if;
      end loop;
   end Get_Prime;


   ---------------------------------------------------------------------------


   function "mod"(Left :  D_Big_Unsigned; Right : Big_Unsigned)
                 return Big_Unsigned;

   ---------------------------------------------------------------------------

   -- Result = Left * Right (mod N)
   function Mult(Left, Right, N : Big_Unsigned) return Big_Unsigned is
      T : D_Mod_Type;
      Carry : Mod_Type := 0;
      R : D_Big_Unsigned;
   begin
      for I in 0..Left.Last_Index loop
         for J in 0..Right.Last_Index loop
            T := D_Mod_Type(Left.Number(I)) *  D_Mod_Type(Right.Number(J))
               + D_Mod_Type(R.Number(I+J)) + D_Mod_Type(Carry);

            R.Number(I+J) := Mod_Type(T and D_Mod_Type(Mod_Type'Last));

            Carry:= Mod_Type(Shift_Right(T,Mod_Type'Size));
         end loop;
         R.Number(I+Right.Last_Index+1) := Carry +
          R.Number(I+Right.Last_Index+1);
         Carry := 0;
      end loop;

     for I in reverse 0..D_Max_Length loop
        if R.Number(I) /= 0 then
            R.Last_Index := I;
            exit;
         end if;
      end loop;
     return R mod N;
   end Mult;

	--------------------------------------------------------------------------

--   function Mult_School(Left, Right, N : Big_Unsigned) return Big_Unsigned is
--   begin
--      return (Left * Right) mod N;
--   end Mult_School;

	--------------------------------------------------------------------------
-- funktioniert nicht

--	   function Barrett(Left, Right, M : Big_Unsigned) return Big_Unsigned is
--      Result : Big_Unsigned;
--      S      : Big_Unsigned;
--		  N      : Natural :=  Bit_Length(Left) ;
--   begin
--      S      := Left * Right;
--      Result := Shift_Right(Shift_Right(S, N) * (2**(Shift_Left(N,1)) / M),N) ;
--      Result := S - (Result * M);
--      return Result;
--   end Barrett;

  ---------------------------------------------------------------------------
-- funktioniert nicht

--   function Montgomery(Left, Right, M : Big_Unsigned) return Big_Unsigned is
--      Result : Big_Unsigned;
--      N      : Natural := Bit_Length(Left);
--      Xi     : Big_Unsigned ;  -- immer null oder eins
--      Leftint : Big_Unsigned := Shift_Left(Left,1);
      
--   begin
   
--      for I in 0..N-1 loop
--        Leftint := Shift_Right(Leftint,1);
--         Xi := Big_Unsigned_One and Leftint;
--         Result := Result + Xi * Right;
--         if (Result.Number(Result.Last_Index) mod 2) = 1 then Result := Result+M; end if;
--         Result := Shift_Right(Result,1) ;
--      end loop; 
--     Result := Montgomery_Helper(Result, (Big_Unsigned_Two**(Shift_Left(N,1)) mod M) , M);
--      return Result;
--   end Montgomery;

--   function Montgomery_Helper(Left, Right, M : Big_Unsigned) return Big_Unsigned is
--      Result : Big_Unsigned;
--      N      : Natural := Bit_Length(Left);
--      Xi     : Big_Unsigned ;  -- immer null oder eins
--      Leftint : Big_Unsigned := Shift_Left(Left,1);
--   begin
--      for I in 0..N-1 loop
--        Leftint := Shift_Right(Leftint,1);
--         Xi := Big_Unsigned_One and Leftint;
--         Result := Result + Xi * Right;
--         if (Result.Number(Result.Last_Index) mod 2) = 1 then Result := Result+M; end if;
--         Result := Shift_Right(Result,1) ;
--      end loop; 
--      return Result;
--   end Montgomery_Helper;
  ---------------------------------------------------------------------------

 ---------------------------------------------------------------------------


   -- Returns a probability  N-bit prime (Result).
   function Get_N_Bit_Prime(N : Positive) return Big_Unsigned  is
      J : Big_Unsigned := Get_Random(Shift_Left(Big_Unsigned_One,N-2));
      Index : constant Natural := (N-1)/Mod_Type'Size;
      Amount : constant Natural := (N-1) mod Mod_Type'Size;
      Result : Big_Unsigned := Shift_Left(J,1);

   begin
      if N = 1 or N > Size then
         raise Constraint_Error;
      end if;


      loop
         -- Make sure that Result is an odd
         Set_Least_Significant_Bit(Result);

         -- Make sure that Result is a N-Bit-Number;
         Result.Number(Index) :=  Result.Number(Index) or
           Shift_Left(Mod_Type(1), Amount);
         if Amount = 0 then Result.Last_Index := Index;
         end if;

         if Is_Prime(Result) then return Result;
         else
            Result:=Result-2;
            if Is_Prime(Result) then return Result;
            end if;
         end if;

         J  := Get_Random(Shift_Left(Big_Unsigned_One,N-2));
         Result := Shift_Left(J,1);
      end loop;

   end Get_N_Bit_Prime;

   ---------------------------------------------------------------------------

   -- computes the jacobi-symbol
   -- return value:
   --   0 : if X mod N = 0
   --   1 : if X is a quadratic resuide mod N
   --  -1 : if X is a quadratic non-resuide mod N

   function Jacobi(X, N : Big_Unsigned) return Integer is
      A : Big_Unsigned :=  X mod N;
   begin

      if Is_Even(N) then
         raise Constraint_Error;
      end if;

      if N = Big_Unsigned_One then return 1;
      elsif A = Big_Unsigned_Zero then return 0;
      elsif A =  Big_Unsigned_One then return 1;
      end if;

      while (A mod 4) = Big_Unsigned_Zero loop
         exit when (A mod 4) = Big_Unsigned_Zero;
         A := Shift_Right(A,2);
      end loop;

      if Is_Even(A) then
         if (N mod 8 = 1) or (N mod 8 = 7) then
            return Jacobi(Shift_Right(A,1),N);
         else return -1*Jacobi(Shift_Right(A,1),N);
         end if;
      else
         if (A mod 4 = 1)  or (N mod 4 = 1) then
            return Jacobi(N mod A, A);
         else  return -1*Jacobi(N mod A, A);
         end if;
      end if;
   end Jacobi;

  ----------------------------------------------------------------------------
  -----------------------------DOUBLE_SIZE------------------------------------
  ----------------------------------------------------------------------------

   --only needed for multiplication mod N
   --here we need 2*Size-bit numbers to avoid an overflow because
   --if one of our provisional result t > BIG_Unsigned_Last
   --then there ist no well known algortihm to compute the
   -- result of an multiplication mod m

   -- same algorithm for D_Big_Unsigned as for Big_Unsigned

   function "="(Left, Right : D_Big_Unsigned) return Boolean is
   begin
      if Left.Last_Index = Right.Last_Index then
         for I in 0..Left.Last_Index loop
            if  Left.Number(I) /= Right.Number(I) then return False;
            end if;
         end loop;
      else return False;
      end if;
      return True;
   end"=";

   ----------------------------------------------------------------------------

   function Shift_Left(Value : D_Big_Unsigned; Amount : Natural)
                      return D_Big_Unsigned is
   begin
      if Amount >= (D_Max_Length+1)*Mod_Type'Size or
        Value = D_Big_Unsigned_Zero
      then  return D_Big_Unsigned_Zero;
      elsif Amount = 0 then return Value;
      end if;

      declare
         Result : D_Big_Unsigned;
         Temp : DD_Mod_Types:=(others => 0);
         L : constant Natural := Amount mod Mod_Type'Size;
         R : constant Natural := Mod_Type'Size-L;
         M : constant Natural := Amount/Mod_Type'Size;
      begin
         Temp(0) := Shift_Left(Value.Number(0), L);

         for I in 1..Value.Last_Index loop
            Temp(I) := Shift_Right(Value.Number(I-1), R) +
              Shift_Left(Value.Number(I), L);
         end loop;

         if Value.Last_Index /= D_Max_Length then
            Temp(Value.Last_Index+1):=
              Shift_Right(Value.Number(Value.Last_Index), R);
         end if;

         for I in Temp'Range loop
            if (I+M) > D_Max_Length then
               exit;
            end if;
            Result.Number(I+M):= Temp(I);
         end loop;

         for I in reverse 0..D_Max_Length loop
            if Result.Number(I) /=0 then
               Result.Last_Index:=I;
               exit;
            end if;
         end loop;
         return Result;
      end;
   end Shift_Left; pragma Inline (Shift_Left);

   ---------------------------------------------------------------------------


   function Bit_Length(X : D_Big_Unsigned) return Natural is
   begin
      if X = D_Big_Unsigned_Zero then
         return 0;
      end if;

      for I in reverse 0..Mod_Type'Size-1 loop
         if Shift_Left(1,I) <= X.Number(X.Last_Index) then
            return  Mod_Type'Size * X.Last_Index + I + 1 ;
         end if;
      end loop;
      return X.Last_Index * Mod_Type'Size;
   end Bit_Length; pragma Inline (Bit_Length);


   ---------------------------------------------------------------------------

   function "<"(Left, Right : D_Big_Unsigned) return Boolean is
   begin
      if Left.Last_Index < Right.Last_Index then return True;
      elsif Left.Last_Index > Right.Last_Index then  return False;
      else
         for I in reverse  0..Left.Last_Index loop
            if  Left.Number(I) < Right.Number(I) then return True;
            elsif Left.Number(I) > Right.Number(I) then return False;
            end if;
         end loop;
      end if;
      return False;
   end "<"; pragma Inline ("<");

   ---------------------------------------------------------------------------

   function ">"(Left, Right : D_Big_Unsigned) return Boolean is
   begin
      return Right < Left;
   end ">"; pragma Inline (">");



   ---------------------------------------------------------------------------

   function ">="(Left, Right : D_Big_Unsigned) return Boolean is
   begin
      return not(Left < Right);
   end ">="; pragma Inline (">=");

   ---------------------------------------------------------------------------

   function "+"(Left, Right : D_Big_Unsigned) return D_Big_Unsigned;

   function "-"(Left, Right : D_Big_Unsigned) return D_Big_Unsigned is
   begin
      if Left = Right then  return D_Big_Unsigned_Zero;
      elsif Left = Right+D_Big_Unsigned_One then return D_Big_Unsigned_One;
      elsif Left+D_Big_Unsigned_One = Right then return D_Big_Unsigned_Last;

      -- add the modulus if Right > Left
      elsif Right > Left then
         return D_Big_Unsigned_Last - Right + Left + D_Big_Unsigned_One;
      else
         declare
            Result : D_Big_Unsigned;
            Carry : Mod_Type:=0;
         begin
            -- Remember Left > Right
            for I in 0..Left.Last_Index loop
               Result.Number(I) := Left.Number(I) - Right.Number(I) - Carry;
               if (Right.Number(I) >  Left.Number(I)) or
                 (Carry= 1 and Right.Number(I) =  Left.Number(I))
               then Carry :=1;
               else Carry :=0;
               end if;
               if Result.Number(I) /= 0 then
                  Result.Last_Index := I;
               end if;
            end loop;
            return Result;
         end;
      end if;
   end "-";


   ---------------------------------------------------------------------------

   function "mod"(Left :  D_Big_Unsigned; Right : Big_Unsigned)
                 return Big_Unsigned is
   begin
      if Left.Last_Index <= Max_Length then
         declare
            L : Big_Unsigned;
         begin
            L.Last_Index := Left.Last_Index;
            L.Number(0..Left.Last_Index) := Left.Number(0..Left.Last_Index);
         return L mod Right;
         end;
      end if;

      if Right = Big_Unsigned_Zero then raise Division_By_Zero;
      --elsif Right = Big_Unsigned_One then return Big_Unsigned_Zero;
      end if;

      -- Now, there is only the case where (Left > Right), (Right /= 0)
      -- and |Left|>|Right|.

      declare
         Remainder : D_Big_Unsigned:=Left;
         Temp_Right, R : D_Big_Unsigned;
         Result : Big_Unsigned;
         Diff: Natural;

      begin
         Temp_Right.Last_Index := Right.Last_Index;
         Temp_Right.Number(0..Right.Last_Index) :=
           Right.Number(0..Right.Last_Index);
         R:=Temp_Right;

         while(Remainder >= R) loop
            Diff := Bit_Length(Remainder) - Bit_Length(R);
            if Diff = 0 then
               Remainder := Remainder-R;
               exit;
            else Diff:=Diff-1;
            end if;
            Temp_Right := Shift_Left(R, Diff);
            Remainder :=  Remainder-Temp_Right;
         end loop;

         Result.Last_Index := Remainder.Last_Index;
         Result.Number(0..Result.Last_Index) :=
           Remainder.Number(0..Result.Last_Index);
         return Result;
      end;
   end "mod";

   ---------------------------------------------------------------------------

   function "+"(Left, Right : D_Big_Unsigned) return D_Big_Unsigned is
      Result : D_Big_Unsigned;
      M : constant Natural  := Natural'Max(Left.Last_Index, Right.Last_Index);
      Temp : Mod_Type;
      Carry : Mod_Type :=0;
   begin

        for I in 0..M loop
         Temp :=Carry;
         Result.Number(I) := Left.Number(I) + Right.Number(I) +Temp;
         if Result.Number(I) < Mod_Type'Max(Left.Number(I), Right.Number(I))
         then  Carry := 1;
         else Carry := 0;
         end if;
      end loop;

      if Carry  =1 and M < Max_Length then
         Result.Number(M+1) := 1;
         Result.Last_Index := M+1;
      else
         -- Set Result.Last_Index
         for I in reverse 0..M loop
            if Result.Number(I) /= 0 then
               Result.Last_Index := I;
               return Result;
            end if;
         end loop;
      end if;
      return Result;
   end "+";

   ---------------------------------------------------------------------------

end Mod_Utils;
