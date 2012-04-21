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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Crypto.Types.Big_Numbers is

  -- package MIO is new Ada.Text_Io.Modular_IO (Mod_Type);

   ---------------------------------------------------------------------------
   -----------------------SEPARATED_BODYS-------------------------------------
   ---------------------------------------------------------------------------

   package body Utils is separate;
   use Utils;

   package body Mod_Utils is separate;
   use Mod_Utils;

   package body Binfield_Utils is separate;
   use Binfield_Utils;

   ---------------------------------------------------------------------------
   ---------------------------COMPARE_FUNCTIONS--------------------------------
   ---------------------------------------------------------------------------


   ---------------------------------------------------------------------------
   --           compare: Big_Unsigned with Big_Unsigned                     --
   ---------------------------------------------------------------------------


   function "="(Left, Right : Big_Unsigned) return Boolean is
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

   ---------------------------------------------------------------------------

   function "<"(Left, Right : Big_Unsigned) return Boolean is
   begin
      if Left.Last_Index < Right.Last_Index then  return True;
      elsif Left.Last_Index > Right.Last_Index then return False;
      else
         for I in reverse  0..Left.Last_Index loop
            if    Left.Number(I) < Right.Number(I) then return True;
            elsif Left.Number(I) > Right.Number(I) then return False;
            end if;
         end loop;
      end if;
      return False;
   end "<";

   ---------------------------------------------------------------------------

   function ">"(Left, Right : Big_Unsigned) return Boolean is
   begin
      return Right < Left;
   end ">";

   ---------------------------------------------------------------------------

   function "<="(Left, Right : Big_Unsigned) return Boolean is
   begin
      return not (Right < Left);
   end "<=";


   ---------------------------------------------------------------------------


   function ">="(Left, Right : Big_Unsigned) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   ---------------------------------------------------------------------------

   function Min(X, Y : in Big_Unsigned) return Big_Unsigned is
   begin
      if (X < Y) then return X;
      else return Y;
      end if;
   end Min;

   ---------------------------------------------------------------------------

   function Max(X, Y : in Big_Unsigned) return Big_Unsigned is
   begin
      if (X <  Y) then return Y;
      else return X;
      end if;
   end Max;


   ---------------------------------------------------------------------------
   --           compare: Big_Unsigned with Mod_Type                         --
   ---------------------------------------------------------------------------


   function "="(Left : Big_Unsigned; Right : Mod_Type) return Boolean is
   begin
      if Left.Last_Index=0 and Left.Number(0) = Right then return True;
      else return False;
      end if;
   end "=";

   ---------------------------------------------------------------------------

   function "="(Left : Mod_Type; Right : Big_Unsigned) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------------------------------------------------------------------------

   function "<"(Left : Big_Unsigned; Right : Mod_Type) return Boolean is
   begin
      if Left.Last_Index > 0 then return False;
      else return Left.Number(Left.Last_Index) < Right;
      end if;
   end "<";

   ---------------------------------------------------------------------------

   function "<"(Left : Mod_Type; Right : Big_Unsigned) return Boolean is
   begin
      if Right.Last_Index > 0 then return True;
      else return Left < Right.Number(Right.Last_Index);
      end if;
   end "<";

   ---------------------------------------------------------------------------

   function ">"(Left : Big_Unsigned; Right : Mod_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   ---------------------------------------------------------------------------

   function ">"(Left : Mod_Type; Right : Big_Unsigned) return Boolean is
   begin
      return Right < Left;
   end ">";

   ---------------------------------------------------------------------------

    function "<="(Left : Big_Unsigned; Right : Mod_Type) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

    ---------------------------------------------------------------------------

    function "<="(Left : Mod_Type; Right : Big_Unsigned) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

    ---------------------------------------------------------------------------

    function ">="(Left : Big_Unsigned; Right : Mod_Type) return Boolean is
    begin
      return not (Left < Right);
   end ">=";

    ---------------------------------------------------------------------------

    function ">="(Left : Mod_Type; Right : Big_Unsigned) return Boolean is
   begin
      return not (Left < Right);
   end ">=";


   ---------------------------------------------------------------------------
   ----------------------------BASE_FUNCTIONS---------------------------------
   ---------------------------------------------------------------------------
--============================================================================--
   function "+"(Left, Right : Big_Unsigned) return Big_Unsigned is
      Result : Big_Unsigned;
      L : constant Natural := Natural'Max( Bit_Length(Left), Bit_Length(Right));
   begin
--      if (L + 1) > Bit_Length(Big_Unsigned_Last) then
--         raise Big_Unsigned_Overflow;
--      elsif L + 1 <= Mod_Type'Size then
--         Result.Number(0) := Left.Number(0) + Right.Number(0);
--      else
      if L + 1 <= Mod_Type'Size then
         Result.Number(0) := Left.Number(0) + Right.Number(0);
      else

         declare
            Carry  : Big_Unsigned;
            Temp   : Big_Unsigned;
         begin
            Carry  := Left and Right;
            Result := Left xor Right;
            Carry  := Shift_Left(Carry,1);
            loop
               Temp   := Result and Carry;
               Result := Result xor Carry;
               Carry  := Temp;
               Carry  := Shift_Left(Carry,1);
               exit when Carry = Big_Unsigned_Zero;
            end loop;
         end;
      end if;

      return Result;
   end "+";

--   function "+"(Left, Right : Big_Unsigned) return Big_Unsigned is
--      Result: Big_Unsigned;
--      Carry : Big_Unsigned;
--      Temp  : Big_Unsigned;
--   begin
--      Carry := Left and Right;
--      Result := Left xor Right;
--      Carry := Shift_Left(Carry,1);
--      --ADA Do_While
--      loop
--         Temp := Result and Carry;
--         Result := Result xor Carry;
--         Carry := Temp;
--         Carry := Shift_Left(Carry,1);
--      exit when Carry = Big_Unsigned_Zero;
--      end loop;
--      return Result;
--   end "+";
--------------------------------------------------------------------------------
--   function "+"(Left, Right : Big_Unsigned) return Big_Unsigned is
--      Result : Big_Unsigned;
--      M : constant Natural  := Natural'Max(Left.Last_Index, Right.Last_Index);
--      Temp : Mod_Type;
--      Carry : Mod_Type :=0;
--   begin
--      for I in 0..M loop
--         Temp :=Carry;
--         Result.Number(I) := Left.Number(I) + Right.Number(I) +Temp;
--         if Result.Number(I) < Mod_Type'Max(Left.Number(I), Right.Number(I))
--         then  Carry := 1;
--         else Carry := 0;
--         end if;
--      end loop;

--      if Carry  =1 and M < Max_Length then
--         Result.Number(M+1) := 1;
--         Result.Last_Index := M+1;
--      else
--         -- Set Result.Last_Index
--         for I in reverse 0..M loop
--            if Result.Number(I) /= 0 then
--               Result.Last_Index := I;
--               return Result;
--            end if;
--         end loop;
--      end if;
--      return Result;
--   end "+";
--============================================================================--
   ---------------------------------------------------------------------------

   function "+"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned is
      Big_Right : Constant Big_Unsigned :=
        (Last_Index => 0, Number => (0 => Right, OTHERS => 0));
   begin
      return Left + Big_Right;
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : Mod_Type; Right : Big_Unsigned) return Big_Unsigned is
      Big_Left : constant Big_Unsigned := (Last_Index => 0, Number => (0 => Left, OTHERS => 0));
   begin
      return Big_Left + Right;
   end "+";

   ---------------------------------------------------------------------------

   function "-"(Left, Right : Big_Unsigned) return Big_Unsigned is
   begin
      -- Add the modulus if Right > Left
      if Right > Left then
         return Big_Unsigned_Last - Right + Left + 1;
--         raise Big_Unsigned_Negative;  -- RSA does not run
      else
         declare
            Result : Big_Unsigned;
            Carry : Mod_Type:=0;
         begin
            -- Remember: Left => Right
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

   function "-"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned is
      Big_Right : constant Big_Unsigned := (Last_Index => 0, Number => (0 => Right, OTHERS => 0));
   begin
     return  Left - Big_Right;
   end "-";

   ---------------------------------------------------------------------------

   function "-"(Left : Mod_Type; Right : Big_Unsigned) return Big_Unsigned is
      Big_Left : constant  Big_Unsigned := (Last_Index => 0, Number => (0 => Left, OTHERS => 0));
   begin
      return Big_Left - Right;
   end "-";

   ---------------------------------------------------------------------------

   function "-"(X : Big_Unsigned) return Big_Unsigned is
   begin
      if X /= Big_Unsigned_Zero then
         return Big_Unsigned_Last-X-1;
      else
         return X;
      end if;
   end "-";

   ---------------------------------------------------------------------------
--============================================================================--

   function "*"(Left, Right : Big_Unsigned) return Big_Unsigned is
      Result : Big_Unsigned ;
      L : constant Natural := Bit_Length(Left)+Bit_Length(Right);
   begin
      if L <= Mod_Type'Size then
         Result.Number(0) := Left.Number(0) * Right.Number(0);
      elsif L > 2800 and L < 3600 then
         Result := Karatsuba_P(Left, Right);
      elsif L > 3600 then
         Result := Toom_Cook_P(Left, Right);
      else

--      if L <= Mod_Type'Size then
--         Result.Number(0) := Left.Number(0) + Right.Number(0);
--      else

         declare
            Temp   : Big_Unsigned;
         begin
            for I in reverse 0..Left.Last_Index loop
               Temp   := Left.Number(I) *  Right;
               Temp   := Shift_Left(Temp, (I*Mod_Type'Size));
               Result := Result + Temp;
            end loop;
         end;
      end if;
--------------------------------------------------------------------------------
--      T : D_Mod_Type;
--      Carry : Mod_Type := 0;
--      R : D_Big_Unsigned;
--      Result : Big_Unsigned;
--      N : constant Natural :=
--        Natural'Min(Left.Last_Index + Right.Last_Index + 1, Max_Length);
--   begin
--      for I in 0..Left.Last_Index loop
--         for J in 0..Right.Last_Index loop
--            T := D_Mod_Type(Left.Number(I)) *  D_Mod_Type(Right.Number(J))
--              + D_Mod_Type(R.Number(I+J)) + D_Mod_Type(Carry);

--            R.Number(I+J) := Mod_Type(T);

--            Carry:= Mod_Type(Shift_Right(T,Mod_Type'Size));
--         end loop;
--         R.Number(I+Right.Last_Index+1) := Carry +
--           R.Number(I+Right.Last_Index+1);
--         Carry := 0;
--      end loop;

--      for I in 0..N loop
--         Result.Number(I) := R.Number(I);
--      end loop;

--      for I in reverse 0..N loop
--         if Result.Number(I) /= 0 then
--            Result.Last_Index := I;
--            exit;
--         end if;
--      end loop;
      return Result;
   end "*";

--------------------------------------------------------------------------------

   function Russ (Left,Right : Big_Unsigned)return Big_Unsigned is
      Result : Big_Unsigned ;
   begin
      if Bit_Length(Left)+Bit_Length(Right) <= Mod_Type'Size then
         Result.Number(0) := Left.Number(0) * Right.Number(0);
      else
         declare
            AA     : Big_Unsigned := Left;
            BB     : Big_Unsigned := Right;
         begin
            while AA > Big_Unsigned_Zero loop
               if (AA and Big_Unsigned_One)  = 1 then
                  Result := Result + BB;
                  AA := AA - 1;
               end if;
               AA := Shift_Right(AA, 1);
               BB := Shift_Left(BB,  1);
           end loop;
         end;
      end if;
      return Result;
   end Russ;

--------------------------------------------------------------------------------

   function Karatsuba (Left, Right : Big_Unsigned) return Big_Unsigned is
         Result           : Big_Unsigned;
   begin
       if Bit_Length(Left)+Bit_Length(Right) < Mod_Type'Size then
          Result.Number(0) := Left.Number(0) * Right.Number(0);
      else
         declare
            Left_1, Left_2   : Big_Unsigned;
            Right_1, Right_2 : Big_Unsigned;
            P_1, P_2 : Big_Unsigned;
            N : constant Natural := Natural'Max( Bit_Length(Left)
                                                     , Bit_Length(Right))/2;
         begin
            Left_1  := Shift_Right(Left, N);
            Left_2  := Left - Shift_Left( Left_1, N );
            Right_1 := Shift_Right(Right, N);
            Right_2 := Right - Shift_Left( Right_1, N );

            P_1     := Left_1 * Right_1;
            P_2     := Left_2 * Right_2;
            Result  :=  Shift_Left(P_1, 2*N)
                     +  Shift_Left(((Left_1  + Left_2)*(Right_1 + Right_2)) - P_1 - P_2, N)
                     +   P_2;
         end;
      end if;
      return Result;
   end Karatsuba;


--------------------------------------------------------------------------------

   function Karatsuba_P (Left, Right : Big_Unsigned) return Big_Unsigned is
      Result           : Big_Unsigned;
   begin
       if Bit_Length(Left)+Bit_Length(Right) < Mod_Type'Size then
          Result.Number(0) := Left.Number(0) * Right.Number(0);
      else
         declare
            Left_1, Left_2   : Big_Unsigned:= Big_Unsigned_Zero;
            Right_1, Right_2 : Big_Unsigned:= Big_Unsigned_Zero;
            P_1, P_2, P_3     : Big_Unsigned:= Big_Unsigned_Zero;
            N : constant Natural := Natural'Max( Bit_Length(Left),
						 Bit_Length(Right))/2;
            -----------------------------------------------------------------------
            task type Karatsuba_Task_Type is
               entry Input (Left, Right : in  Big_Unsigned);
               entry Output(Result      : out Big_Unsigned);
            end Karatsuba_Task_Type;
            task body Karatsuba_Task_Type is
               X           : Big_Unsigned;
               Left_Local  : Big_Unsigned;
               Right_Local : Big_Unsigned;
            begin
               accept Input (Left, Right : Big_Unsigned) do
                  Left_Local  := Left;
                  Right_Local := Right;
               end Input;

--               X := Karatsuba(Left_Local, Right_Local);
               X := Left_Local * Right_Local;

               accept Output(Result      : out Big_Unsigned) do
                   Result := X;
               end Output;
            end Karatsuba_Task_Type;
            Task_1 : Karatsuba_Task_Type;
            Task_2 : Karatsuba_Task_Type;
            Task_3 : Karatsuba_Task_Type;
            -----------------------------------------------------------------------
         begin
            Left_1  := Shift_Right(Left, N);
            Left_2  := Left - Shift_Left( Left_1, N );
            Right_1 := Shift_Right(Right, N);
            Right_2 := Right - Shift_Left( Right_1, N );

            Task_1.Input(Left_1, Right_1);
            Task_2.Input(Left_2, Right_2);
            Task_3.Input((Left_1  + Left_2), (Right_1 + Right_2));

            Task_1.Output(Result => P_1);
            Task_2.Output(Result => P_2);
            Task_3.Output(Result => P_3);

            Result :=  Shift_Left(P_1, 2*N)
                    +  Shift_Left((P_3 - P_1 - P_2), N)
                    +   P_2;
         end;
      end if;
      return Result;
   end Karatsuba_P;
   
   -----------------------------------------------------------------------
   
   function Toom_Cook(Left, Right : Big_Unsigned) return Big_Unsigned is
      Result              : Big_Unsigned;
   begin
      if Bit_Length(Left)+Bit_Length(Right) < Mod_Type'Size then
          Result.Number(0) := Left.Number(0) * Right.Number(0);
      else

      declare
         knuth_1             : Array (1..5) of Big_Unsigned;
         knuth_2             : Array (1..4) of Big_Unsigned;
         knuth_3             : Array (1..3) of Big_Unsigned;
         knuth_4             : Array (1..2) of Big_Unsigned;
         knuth_5_1 : Big_Unsigned;
         L, R                : Array (0..3) of Big_Unsigned;
         F_Left, F_Right     : Array (2..4) of Big_Unsigned;
         Z                   : Array (0..4) of Big_Unsigned;
         Length : constant Natural  := Natural'Max( Bit_Length(Left) ,
						    Bit_Length(Right) );
         N  : constant Natural  := Length / 3;
         DN : constant Natural  := 2 * N;

      begin

      -- SPLITTING .............................................................

         L(0) := Shift_Right( Left, DN);
         L(1) := Shift_Right( Left, N ) - Shift_Left( L(0), N );
         L(2) := Left - Shift_Left( L(0), DN ) - Shift_Left( L(1), N );
         R(0) := Shift_Right( Right, DN);
         R(1) := Shift_Right( Right, N ) - Shift_Left( R(0), N );
         R(2) := Right - Shift_Left( R(0), DN ) - Shift_Left( R(1), N );

         F_Left(2)  := Shift_Left(L(0),2)  + Shift_Left(L(1),1) + L(2);
         F_Right(2) := Shift_Left(R(0),2)  + Shift_Left(R(1),1) + R(2);
         F_Left(3)  := (Shift_Left(L(0),3) + L(0)) + (Shift_Left(L(1),1)+L(1)) + L(2);
         F_Right(3) := (Shift_Left(R(0),3) + R(0)) + (Shift_Left(R(1),1)+R(1)) + R(2);
         F_Left(4)  := Shift_Left(L(0),4)  + Shift_Left(L(1),2) + L(2);
         F_Right(4) := Shift_Left(R(0),4)  + Shift_Left(R(1),2) + R(2);

   -- INTERPOLATION with POINTWISE MULT .....................................

         knuth_1(1) :=       L(2)* R(2);
         knuth_1(2) := (L(0) + L(1) + L(2)) * (R(0) + R(1) + R(2));
         knuth_1(3) := F_Left(2) * F_Right(2);
         knuth_1(4) := F_Left(3) * F_Right(3);
         knuth_1(5) := F_Left(4) * F_Right(4);

         knuth_2(1) := knuth_1(2) - knuth_1(1);
         knuth_2(2) := knuth_1(3) - knuth_1(2);
         knuth_2(3) := knuth_1(4) - knuth_1(3);
         knuth_2(4) := knuth_1(5) - knuth_1(4);
         knuth_3(1) := Shift_Right((knuth_2(2) - knuth_2(1)),1);
         knuth_3(2) := Shift_Right((knuth_2(3) - knuth_2(2)),1);
         knuth_3(3) := Shift_Right((knuth_2(4) - knuth_2(3)),1);
         knuth_4(1) := (knuth_3(2) - knuth_3(1)) / 3;
         knuth_4(2) := (knuth_3(3) - knuth_3(2)) / 3;
         knuth_5_1  := Shift_Right(knuth_4(2) - knuth_4(1),2);

         Z(0) := knuth_5_1;
         Z(1) := knuth_4(1);
         Z(2) := knuth_3(1);
         Z(3) := knuth_2(1);
         Z(4) := knuth_1(1);

   -- RECOMPOSITION ............................................................

         knuth_1(1) :=       Z(1) - (Z(0) + Shift_Left(Z(0),1));
         knuth_1(2) := knuth_1(1) - (Shift_Left(Z(0),1));
         knuth_1(3) := knuth_1(2) - Z(0);
         knuth_2(1) :=       Z(2) - (Shift_Left(knuth_1(1),1));
         knuth_2(2) := knuth_2(1) - knuth_1(2);
         knuth_3(1) :=       Z(3) - knuth_2(1);

         Result := Shift_Left(      Z(0), Shift_Left(N,2))
                 + Shift_Left(knuth_1(3), Shift_Left(N,1)+N)
                 + Shift_Left(knuth_2(2), Shift_Left(N,1))
                 + Shift_Left(knuth_3(1), N)
                 + Z(4);
         end;
         end if;
      return Result;
   end Toom_Cook;

--------------------------------------------------------------------------------

   function Toom_Cook_P(Left, Right : Big_Unsigned) return Big_Unsigned is
      Result              : Big_Unsigned;
   begin
      if Bit_Length(Left)+Bit_Length(Right) < Mod_Type'Size then
          Result.Number(0) := Left.Number(0) * Right.Number(0);
      else
         declare
            knuth_1             : Array (1..5) of Big_Unsigned;
            knuth_2             : Array (1..4) of Big_Unsigned;
            knuth_3             : Array (1..3) of Big_Unsigned;
            knuth_4             : Array (1..2) of Big_Unsigned;
            knuth_5_1 : Big_Unsigned;
            L, R                : Array (0..3) of Big_Unsigned;
            F_Left, F_Right     : Array (2..4) of Big_Unsigned;
            Z                   : Array (0..4) of Big_Unsigned;
	    
            Length : constant Natural  := Natural'Max(Bit_Length(Left) ,
						      Bit_Length(Right) );
            N : constant Natural  := Length / 3;
            DN : constant Natural  := 2 * N;

            task type Toom_Cook_Task_Type is
               entry Input (Left, Right : in  Big_Unsigned);
               entry Output(Result      : out Big_Unsigned);
            end Toom_Cook_Task_Type;
            task body Toom_Cook_Task_Type is
               X           : Big_Unsigned;
               Left_Local  : Big_Unsigned;
               Right_Local : Big_Unsigned;
            begin
               accept Input (Left, Right : Big_Unsigned) do
                  Left_Local  := Left;
                  Right_Local := Right;
               end Input;

               X := Left_Local * Right_Local;

               accept Output(Result      : out Big_Unsigned) do
                   Result := X;
               end Output;
            end Toom_Cook_Task_Type;

            Task_1 : Toom_Cook_Task_Type;
            Task_2 : Toom_Cook_Task_Type;
            Task_3 : Toom_Cook_Task_Type;
            Task_4 : Toom_Cook_Task_Type;
            Task_5 : Toom_Cook_Task_Type;

         begin
      -- SPLITTING .............................................................
            L(0) := Shift_Right( Left, DN);
            L(1) := Shift_Right( Left, N ) - Shift_Left( L(0), N );
            L(2) := Left - Shift_Left( L(0), DN ) - Shift_Left( L(1), N );
            R(0) := Shift_Right( Right, DN);
            R(1) := Shift_Right( Right, N ) - Shift_Left( R(0), N );
            R(2) := Right - Shift_Left( R(0), DN ) - Shift_Left( R(1), N );
      -- EVALUATION ............................................................
            F_Left(2)  := Shift_Left(L(0),2)  + Shift_Left(L(1),1) + L(2);
            F_Right(2) := Shift_Left(R(0),2)  + Shift_Left(R(1),1) + R(2);
            F_Left(3)  := (Shift_Left(L(0),3) + L(0))
                        + (Shift_Left(L(1),1)+L(1)) + L(2);
            F_Right(3) := (Shift_Left(R(0),3) + R(0))
                        + (Shift_Left(R(1),1)+R(1)) + R(2);
            F_Left(4)  := Shift_Left(L(0),4)  + Shift_Left(L(1),2) + L(2);
            F_Right(4) := Shift_Left(R(0),4)  + Shift_Left(R(1),2) + R(2);
      -- INTERPOLATION with POINTWISE MULT .....................................
            Task_1.Input(     L(2), R(2)      );
            Task_2.Input(L(0) + L(1) + L(2), R(0) + R(1) + R(2));
            Task_3.Input(F_Left(2), F_Right(2));
            Task_4.Input(F_Left(3), F_Right(3));
            Task_5.Input(F_Left(4), F_Right(4));
            Task_1.Output(Result => knuth_1(1));
            Task_2.Output(Result => knuth_1(2));
            Task_3.Output(Result => knuth_1(3));
            Task_4.Output(Result => knuth_1(4));
            Task_5.Output(Result => knuth_1(5));

            knuth_2(1) := knuth_1(2) - knuth_1(1);
            knuth_2(2) := knuth_1(3) - knuth_1(2);
            knuth_2(3) := knuth_1(4) - knuth_1(3);
            knuth_2(4) := knuth_1(5) - knuth_1(4);
            knuth_3(1) := Shift_Right((knuth_2(2) - knuth_2(1)),1);
            knuth_3(2) := Shift_Right((knuth_2(3) - knuth_2(2)),1);
            knuth_3(3) := Shift_Right((knuth_2(4) - knuth_2(3)),1);
            knuth_4(1) := (knuth_3(2) - knuth_3(1)) / 3;
            knuth_4(2) := (knuth_3(3) - knuth_3(2)) / 3;
            knuth_5_1  := Shift_Right(knuth_4(2) - knuth_4(1),2);

            Z(0) := knuth_5_1;
            Z(1) := knuth_4(1);
            Z(2) := knuth_3(1);
            Z(3) := knuth_2(1);
            Z(4) := knuth_1(1);
      -- RECOMPOSITION .........................................................
            knuth_1(1) :=       Z(1) - (Z(0) + Shift_Left(Z(0),1));
            knuth_1(2) := knuth_1(1) - (Shift_Left(Z(0),1));
            knuth_1(3) := knuth_1(2) - Z(0);
            knuth_2(1) :=       Z(2) - (Shift_Left(knuth_1(1),1));
            knuth_2(2) := knuth_2(1) - knuth_1(2);
            knuth_3(1) :=       Z(3) - knuth_2(1);

            Result := Shift_Left(      Z(0), Shift_Left(N,2))
                    + Shift_Left(knuth_1(3), Shift_Left(N,1)+N)
                    + Shift_Left(knuth_2(2), Shift_Left(N,1))
                    + Shift_Left(knuth_3(1), N)
                    + Z(4);
            end;
         end if;
      return Result;
   end Toom_Cook_P;

--============================================================================--
   ---------------------------------------------------------------------------

   function "*"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned is
   begin
      if Right = 0 or Left = Big_Unsigned_Zero  then return Big_Unsigned_Zero;
      elsif Right = 1 then return Left;
      end if;

      declare
         Result : Big_Unsigned;
      begin
         for I in 0..Mod_Type'Size loop
            if (Shift_Right(Right,I) mod 2) = 1 then
               Result:= Result + Shift_Left(Left,I);
            end if;
         end loop;
         return Result;
      end;
   end "*";

   ---------------------------------------------------------------------------

   function "*"(Left : Mod_Type; Right : Big_Unsigned) return Big_Unsigned is
   begin
      return Right * Left;
   end "*";

   ---------------------------------------------------------------------------

   function "**"(Left, Right : Big_Unsigned) return Big_Unsigned is
   begin
      if Left = Big_Unsigned_Zero or Left = Big_Unsigned_One then
         return Left;
      end if;

      -- Square_And_Multiply
      declare
         Result : Big_Unsigned := Big_Unsigned_One;
      begin
         for I in reverse  0..Bit_Length(Right)-1 loop
            Result := Result * Result;
            if (Shift_Right(Right, I) mod 2) = Big_Unsigned_One then
               Result := Result * Left;
            end if;
         end loop;
         return Result;
      end;
   end "**";

   ---------------------------------------------------------------------------


   function "/"(Left, Right : Big_Unsigned) return Big_Unsigned is
      Q : Big_Unsigned;
      R : Big_Unsigned;
   begin
      Big_Div(Left,Right,Q,R);
      return Q;
   end "/";

   ---------------------------------------------------------------------------

   function "/"(Left : Mod_Type; Right : Big_Unsigned) return Big_Unsigned is
      Big_Left: constant Big_Unsigned :=
        (Last_Index => 0, Number => (0=> Left, others => 0));
      Q : Big_Unsigned;
      R : Big_Unsigned;
   begin
      Big_Div(Big_Left,Right,Q,R);
      return Q;
   end "/";

   ---------------------------------------------------------------------------

   function "/"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned is
      Q : Big_Unsigned;
      R : Mod_Type;
   begin
      Short_Div(Left,Right,Q,R);
      return Q;
   end "/";


   ---------------------------------------------------------------------------

   function "mod"(Left, Right : Big_Unsigned) return Big_Unsigned is
      Q : Big_Unsigned;
      R : Big_Unsigned;
   begin
      Big_Div(Left,Right,Q,R);
      return R;
   end "mod";



   ---------------------------------------------------------------------------

   function "mod"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned is
      Q : Big_Unsigned;
      R : Mod_Type;
   begin
      Short_Div(Left,Right,Q,R);
      declare
         Result: constant Big_Unsigned :=
           (Last_Index => 0, Number => (0 => R, others => 0));
      begin
         return Result;
      end;
   end "mod";

   ---------------------------------------------------------------------------

    -- This is a helper function
    -- This procedure computes/adjust the Last_Index of B
    procedure Last_Index(B : in out Big_Unsigned; M : in M_Len:=Max_Length) is
    begin
       for I in  reverse 0..M loop
          if B.Number(I) /= 0 then
            B.Last_Index := I;
            exit;
          end if;
       end loop;
    end Last_Index; pragma Inline (Last_Index);

   ---------------------------------------------------------------------------


    function "xor"(Left, Right : Big_Unsigned) return Big_Unsigned is
       Result : Big_Unsigned;
       M : constant Natural:= Natural'Max(Left.Last_Index, Right.Last_Index);
    begin
       -- xor
       for I in  0..M loop
          Result.Number(I) := Left.Number(I) xor Right.Number(I);
       end loop;

       -- compute the Last_Index
       Last_Index(Result,M);

       return Result;

    end "xor";

   ---------------------------------------------------------------------------

    function "and"(Left, Right : Big_Unsigned) return Big_Unsigned is
       Result : Big_Unsigned;
       M : constant Natural:= Natural'Min(Left.Last_Index, Right.Last_Index);
    begin

       --and
       for I in  0..M loop
          Result.Number(I) := Left.Number(I) and Right.Number(I);
       end loop;

       -- compute last index
       Last_Index(Result, M);

       return Result;
    end "and";

    ---------------------------------------------------------------------------


    function "and"(Left : Big_Unsigned; Right : Mod_Type) return Big_Unsigned
    is
       Result : Big_Unsigned;
    begin

       Result.Number(0) := Left.Number(0) and Right;

       -- compute last index
       Last_Index(Result, 0);

       return Result;
    end "and";

    ---------------------------------------------------------------------------

    function "and"(Left : Mod_Type; Right : Big_Unsigned) return Big_Unsigned
    is
       Result : Big_Unsigned;
    begin

       Result.Number(0) :=  Left and Right.Number(0);

       -- compute last index
       Last_Index(Result, 0);

       return Result;
    end "and";

    ---------------------------------------------------------------------------


  function "or"(Left, Right : Big_Unsigned) return Big_Unsigned is
     Result : Big_Unsigned;
     M : constant Natural:= Natural'Max(Left.Last_Index, Right.Last_Index);
  begin
     -- or
     for I in  0..M loop
        Result.Number(I) := Left.Number(I) or Right.Number(I);
     end loop;

     -- compute last index
     Last_Index(Result, M);

     return Result;
  end "or";

  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------

begin
   if Size mod Mod_Type'Size /= 0 then
     Put("Size must be a multiple of " & Mod_Type'Image(Mod_Type'Size));
     raise Constraint_Size_Error;
   end if;
end Crypto.Types.Big_Numbers;
