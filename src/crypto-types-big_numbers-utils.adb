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

--with Ada.Integer_Text_IO;
--with Ada.Strings.Unbounded.Text_IO;
with Crypto.Types.Random;

SEPARATE(Crypto.Types.Big_Numbers)

package body Utils is

   pragma Optimize (Time);

   ---------------------------------------------------------------------------

   procedure Swap(X, Y : in out Big_Unsigned) is
      Temp : constant Big_Unsigned := X;
   begin
      X := Y;
      Y := Temp;
   end Swap; pragma Inline (Swap);

   ---------------------------------------------------------------------------

   procedure Set_Least_Significant_Bit(X : in out Big_Unsigned) is
   begin
      X.Number(0) := X.Number(0) or 1;
   end Set_Least_Significant_Bit; pragma Inline(Set_Least_Significant_Bit);

  ---------------------------------------------------------------------------

   function Is_Odd(X : Big_Unsigned) return Boolean  is
   begin
      if (X.Number(0) and 1) = 1 then return True;
      else return False;
      end if;
   end Is_Odd; pragma Inline(Is_Odd);

   ---------------------------------------------------------------------------

   function Is_Even(X : Big_Unsigned) return Boolean  is
   begin
      if (X.Number(0) and 1) = 0 then return True;
      else return False;
      end if;
   end Is_Even; pragma Inline(Is_Even);

   ---------------------------------------------------------------------------

   procedure Set_Most_Significant_Bit(X : in out Big_Unsigned) is
   begin
      X.Last_Index := Max_Length;
      X.Number(Max_Length) := X.Number(Max_Length) or
        Shift_Left(Word(1), Word'Size-1);
   end Set_Most_Significant_Bit; pragma Inline(Set_Most_Significant_Bit);


   ---------------------------------------------------------------------------

   function Bit_Length(X : Big_Unsigned) return Natural is
   begin
      if X = Big_Unsigned_Zero then
         return 0;
      end if;

      for I in reverse 0..Word'Size-1 loop
         if Shift_Left(1,I) <= X.Number(X.Last_Index) then
            return  Word'Size * X.Last_Index + I + 1 ;
         end if;
      end loop;
      return X.Last_Index * Word'Size;
   end Bit_Length;  pragma Inline(Bit_Length);

   ---------------------------------------------------------------------------

   function Lowest_Set_Bit(X : Big_Unsigned) return Natural is
   begin
      if X = Big_Unsigned_Zero then
         raise Is_Zero_Error;
      end if;

      for I in 0..X.Last_Index loop
         if X.Number(I) /= 0 then
            for J in  0..Word'Size-1 loop
               if (Shift_Right(X.Number(I),J) and 1) = 1 then
                  return I*Word'Size+J+1;
               end if;
            end loop;
         end if;
      end loop;
      return Size+1;  --X = Big_unsgned_Zero = 2**(Size+1)
   end Lowest_Set_Bit; pragma Inline (Lowest_Set_Bit);


   ---------------------------------------------------------------------------


   procedure Inc(X : in out Big_Unsigned) is
   begin
      if X = Big_Unsigned_Last then
         X := Big_Unsigned_Zero;
      else
         X.Number(0) := X.Number(0) + 1;
         for I in 0..X.Last_Index loop
            if X.Number(I) /= 0 then
               exit;
            else X.Number(I+1) := X. Number(I+1) + 1;
            end if;
         end loop;

         -- if an mod_type overflow occure then we have some extra work do
         if X.Number(X.Last_Index) = 0 then
            X.Last_Index := X.Last_Index + 1;
         end if;
      end if;
   end Inc; pragma Inline(Inc);

   ---------------------------------------------------------------------------

   procedure Dec(X : in out Big_Unsigned) is
   begin
      if X = Big_Unsigned_Zero then
         X := Big_Unsigned_Last;
      else
         X.Number(0) := X.Number(0) - 1;
         for I in 0..X.Last_Index loop
            if X.Number(I) /= Word'Last then
               exit;
            else X.Number(I+1) := X.Number(I+1) - 1;
            end if;
         end loop;


         -- check if we must dec the Last_index too
         if X.Number(X.Last_Index) = 0 and X.Last_Index /= 0 then
            X.Last_Index := X.Last_Index - 1;
         end if;
      end if;
   end Dec; pragma Inline(Dec);

   ---------------------------------------------------------------------------

   function Shift_Left(Value : Big_Unsigned; Amount : Natural)
                      return Big_Unsigned is
   begin
      if Amount >= (Max_Length+1)*Word'Size or Value = Big_Unsigned_Zero
      then  return Big_Unsigned_Zero;
      elsif Amount = 0 then return Value;
      end if;

      declare
         Result : Big_Unsigned;
         Temp : Limbs:=(others => 0);
         L : constant Natural := Amount mod Word'Size;
         R : constant Natural := Word'Size-L;
         M : constant Natural := Amount/Word'Size;
      begin
         Temp(0) := Shift_Left(Value.Number(0), L);

--         for I in 1..Value.Last_Index loop
--            Temp(I) := Shift_Right(Value.Number(I-1), R) +
--              Shift_Left(Value.Number(I), L);
--         end loop;
         for I in 1..Value.Last_Index loop
            Temp(I) := Shift_Right(Value.Number(I-1), R) xor
              Shift_Left(Value.Number(I), L);
         end loop;

         if Value.Last_Index /= Max_Length then
            Temp(Value.Last_Index+1):=
              Shift_Right(Value.Number(Value.Last_Index), R);
         end if;

         for I in Temp'Range loop
            if (I+M) > Max_Length then
               exit;
            end if;
            Result.Number(I+M):= Temp(I);
         end loop;
         for I in reverse 0..Max_Length loop
            if Result.Number(I) /=0 then
               Result.Last_Index:=I;
               exit;
            end if;
         end loop;
         return Result;
      end;
   end Shift_Left;  -- pragma Inline (Shift_Left);

   ---------------------------------------------------------------------------

   function Shift_Right(Value : Big_Unsigned; Amount : Natural)
                       return Big_Unsigned is
   begin
      if Amount >= (Max_Length+1)*Word'Size or Value = Big_Unsigned_Zero
      then  return Big_Unsigned_Zero;
      elsif Amount = 0 then return Value;
      end if;

      declare
         Result : Big_Unsigned:=Big_Unsigned_Zero;
         Temp : Limbs :=(others => 0);
         R : constant Natural := Amount mod Word'Size;
         L : constant Natural := Word'Size-R;
         M : constant Natural := Amount/Word'Size;
      begin
         Temp(Value.Last_Index) :=
           Shift_Right(Value.Number(Value.Last_Index), R);

--         for I in reverse 0..Value.Last_Index-1 loop
--            Temp(I) := Shift_Left(Value.Number(I+1), L) +
--                 Shift_Right(Value.Number(I), R);
--         end loop;
         for I in reverse 0..Value.Last_Index-1 loop
            Temp(I) := Shift_Left(Value.Number(I+1), L) xor
                 Shift_Right(Value.Number(I), R);
         end loop;

         for I in reverse Temp'Range loop
            if (I-M) < 0 then
               exit;
            end if;
            Result.Number(I-M):= Temp(I);
         end loop;

         for I in reverse 0..Value.Last_Index loop
            if Result.Number(I) /= 0 or I = 0 then
               Result.Last_Index := I;
               exit;
            end if;
         end loop;
         return Result;
      end;
   end Shift_Right; --pragma Inline (Shift_Right);


   ---------------------------------------------------------------------------

   function Rotate_Left(Value : Big_Unsigned; Amount : Natural)
                       return Big_Unsigned is
      L : constant Natural := Amount mod Size;
   begin
      if Value = Big_Unsigned_Last then
         return Big_Unsigned_Last;
      end if;
      return Shift_Left(Value,L) xor Shift_Right(Value, Size-L);
   end Rotate_Left;  pragma Inline (Rotate_Left);

   ---------------------------------------------------------------------------

   function Rotate_Right(Value : Big_Unsigned; Amount : Natural)
                        return Big_Unsigned is
      R : constant Natural := Amount mod Size;
   begin
      if Value = Big_Unsigned_Last then
         return Big_Unsigned_Last;
      end if;
      return Shift_Right(Value,R) xor Shift_Left(Value, Size-R);
   end Rotate_Right; pragma Inline (Rotate_Right);

   ---------------------------------------------------------------------------

   function Gcd(Left, Right : Big_Unsigned) return Big_Unsigned is
      A : Big_Unsigned := Max(Left,Right);
      B : Big_Unsigned := Min(Left,Right);
      R : Big_Unsigned;
   begin
      while B /= Big_Unsigned_Zero  loop
         R := A mod B;
         A := B;
         B := R;
      end loop;
      return A;
   end Gcd; pragma Inline (Gcd);

   ---------------------------------------------------------------------------

   function Get_Random return Big_Unsigned is
      Result : Big_Unsigned;
   begin
      Random.Read(Result.Number);
      return Result;
   end Get_Random; pragma Inline (Get_Random);

   ---------------------------------------------------------------------------
   
   function Length_In_Bytes(X : Big_Unsigned) return Natural is
      Len : constant Natural := Bit_Length(X);
   begin
      if Len mod Byte'Size = 0 then return (Len / Byte'Size);
      else return (Len / Byte'Size) + 1;
      end if;
   end Length_In_Bytes; pragma Inline (Length_In_Bytes);

   ---------------------------------------------------------------------------

   function To_Words(X : Big_Unsigned) return Words  is
   begin
      return X.Number(0..X.Last_Index);
   end To_Words;  pragma Inline (To_Words);


   ---------------------------------------------------------------------------
   
   function Max(Left, Right : Integer) return Integer is
   begin
      if Left < Right then
	 return Right;
      else
	 return Left;
      end if;
   end Max;
     
   
   ---------------------------------------------------------------------------

   function To_Bytes(X : Big_Unsigned) return Bytes is
      L : constant Natural := Max(Length_In_Bytes(X)-1,0);
      M : constant Natural := 3; --(Word'Size / Byte'Size) - 1;
      E : constant Integer := ((L+1) mod 4) - 1;
      B : Bytes(0..L);
   begin
      for I in  0..X.Last_Index-1 loop
         for J in 0..M loop
            B(L-I*(M+1)-J) := Byte(Shift_Right(X.Number(I), J*Byte'Size) and
                                   Word(Byte'Last));
         end loop;
      end loop;

      if E >= 0 then
         for I in 0..E loop
            B(I) := Byte(Shift_Right(X.Number(X.Last_Index), (E-I)*Byte'Size)
                         and Word(Byte'Last));
         end loop;
      else
         for J in 0..M loop
            B(M-J) := Byte(Shift_Right(X.Number(X.Last_Index), J*Byte'Size)
                           and Word(Byte'Last));
         end loop;
      end if;

      return B;
   end To_Bytes;

   ---------------------------------------------------------------------------

   function To_Big_Unsigned(X : Words) return Big_Unsigned is
      Result : Big_Unsigned;
   begin
      if X'Length > Max_Length then
         raise Constraint_Error;
      else
         Result.Number(0..X'Last-X'First)  := X;
      end if;

      for I in reverse 0..Max_Length loop
         if Result.Number(I) /= 0 then
            Result.Last_Index := I;
            exit;
         end if;
      end loop;

      return Result;
   end To_Big_Unsigned;

   ---------------------------------------------------------------------------

   function To_Big_Unsigned(X : Bytes) return Big_Unsigned is
     Result : Big_Unsigned;
     M : constant Natural := Word'Size / Byte'Size; -- Bytes per Word
     Shift_Amount, counter : Natural:=0;
   begin
      if X'Length*Byte'Size > Size then
         raise Constraint_Error;
      end if;

      for I in reverse X'Range loop
         Result.Number(Counter/M) := Result.Number(Counter/M) or
           Shift_Left(Word(X(I)), Shift_Amount*Byte'Size);
         Shift_Amount := (Shift_Amount + 1) mod M;
         Counter:=Counter+1;
      end loop;

      for I in reverse 0..Max_Length loop
         if Result.Number(I) /= 0 then
            Result.Last_Index := I;
            exit;
         end if;
      end loop;

      return Result;

   end To_Big_Unsigned;

   ---------------------------------------------------------------------------

   procedure Big_Div(Dividend, Divisor : in  Big_Unsigned;
                     Quotient, Remainder  : out Big_Unsigned) is
      Last_Divisor : constant Natural := Divisor.Last_Index;
   begin
      if (Last_Divisor = 0) then
         case Divisor.Number(0) is
            when 0 => raise Division_By_Zero;
            when 1 => Quotient := Dividend;
               Remainder := Big_Unsigned_Zero;
               return;
            when others => declare
               Temp_Remainder : Word;
               Temp_Divisor : constant Word := Divisor.Number(0);
            begin
               -- We use the function Short_Div, which is faster.
               -- See below for the implementation of Short_Div.
               Short_Div(Dividend, Temp_Divisor, Quotient, Temp_Remainder);
               Remainder := (Last_Index => 0,
                             Number => (Temp_Remainder, others => 0));
               return;
            end;
         end case;

      elsif (Dividend < Divisor) then
         Quotient  := Big_Unsigned_Zero;
         Remainder := Dividend;
         return;

      elsif Dividend = Big_Unsigned_Zero then
         Quotient  := Big_Unsigned_Zero;
         Remainder := Big_Unsigned_Zero;
         return;

      elsif (Bit_Length(Dividend) = Bit_Length(Divisor)) then
         -- Dividend > Divisor and Divisor /= 0 and
         -- |Dividend|=|Divisor| => Dividend/Divisor=1
         Quotient:=Big_Unsigned_One;
         Remainder:=Dividend-Divisor;
         return;
      end if;

      -- Now, there is only the case where (Dividend > Divisor), (Divisor /= 0)
      -- and |Dividend|>|Divisor|.

      declare
         Temp_Divisor: Big_Unsigned :=Divisor;
         Diff: Natural;
      begin
         Remainder:= Dividend;
         Quotient:=Big_Unsigned_Zero;

         while(Remainder >= Divisor) loop
            Diff := Bit_Length(Remainder) - Bit_Length(Divisor);
            if Diff = 0 then
               Quotient:=Quotient+1;
               Remainder:=Remainder-Divisor;
               return;
            else Diff:=Diff-1;
            end if;
            Temp_Divisor := Shift_Left(Divisor, Diff);
            Remainder := Remainder-Temp_Divisor;
            Quotient := Quotient + Shift_Left(Big_Unsigned_One, Diff);
         end loop;
      end;
   end Big_Div;


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Short_Div(Dividend  : in  Big_Unsigned;
                       Divisor   : in  Word;
                       Quotient  : out Big_Unsigned;
                       Remainder : out Word) is
   begin

      -- simple cases
      if (Dividend < Divisor) then
         Remainder := Dividend.Number(0);
         Quotient  := Big_Unsigned_Zero;
         return;
      elsif (Divisor = 0) then
         raise Division_By_Zero;
      elsif (Divisor = 1) then
         Quotient := Dividend;
         Remainder := 0;
         return;
      elsif (Dividend = Divisor) then
         Quotient := Big_Unsigned_One;
         Remainder := 0;
         return;
      end if;

      declare
         Last_Dividend : constant Natural := Dividend.Last_Index;
         Temp_Quotient : Big_Unsigned;
         Carry : Largest_Unsigned := 0;
         Temp : Largest_Unsigned;
         Temp_Divisor : constant Largest_Unsigned :=
           Largest_Unsigned(Divisor);

      begin
         for I in reverse 0..Last_Dividend loop
            Temp := Largest_Unsigned(Dividend.Number(I))
              + Shift_Left(Carry, Word'Size);
            Temp_Quotient.Number(I) := Word(Temp / Temp_Divisor);
            Carry := Temp mod Temp_Divisor;
         end loop;

         if (Last_Dividend > 0) and then
           (Temp_Quotient.Number(Last_Dividend) = 0) then
            Temp_Quotient.Last_Index := Last_Dividend - 1;
         else
            Temp_Quotient.Last_Index := Last_Dividend;
         end if;
         Quotient := Temp_Quotient;
         Remainder := Word(Carry);
      end;
   end Short_Div;


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

--   package IIO renames Ada.Integer_Text_IO;
--   package UIO renames Ada.Strings.Unbounded.Text_IO;

   ---------------------------------------------------------------------------

   function To_String(Item : Big_Unsigned;
                                   Base : Number_Base := 10) return String is
      S  : Unbounded_String  := Null_Unbounded_String;
      Remainder : Word:=0;
      Temp_Item : Big_Unsigned := Item;
      Trans : constant array(Word range 0..15) of Character :=
        ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
   begin
      if Item = Big_Unsigned_Zero then
         if Base = 10 then  return "0";
         else
            S := Base'Img & "#0#" & S;
            return Slice(S,2,Length(S));
         end if;
      else
         if Base /= 10 then
            S := "#" & S;
         end if;
         while (Temp_Item /= Big_Unsigned_Zero) loop
            Short_Div(Temp_Item, Word(Base), Temp_Item, Remainder);
            S := Trans(Remainder) & S;
         end loop;
         if Base /= 10 then
            S := "#" & S;
            S := Base'Img & S;
            return Slice(S,2,Length(S));
         end if;
      end if;
      return To_String(S);
      end To_String;

   ---------------------------------------------------------------------------

   procedure Put(Item : in Big_Unsigned; Base : in Number_Base := 10) is
   begin
      Put(To_String(Item, Base));
   end Put; --pragma Inline(Put);

   ---------------------------------------------------------------------------

    procedure Put_Line(Item : in Big_Unsigned; Base : in Number_Base := 10) is
   begin
      Put(To_String(Item, Base)); New_Line;
   end Put_Line; --pragma Inline(Put_Line);

   ---------------------------------------------------------------------------

   function Get_Digit(C : Character) return Word  is
   begin
      case C is
         when '0'..'9' => return Character'Pos(C) - Character'Pos('0');
         when 'A'..'F' => return Character'Pos(C) - Character'Pos('A') + 10;
         when others =>  raise Conversion_Error;
      end case;
   end Get_Digit; pragma Inline(Get_Digit);

   ---------------------------------------------------------------------------

   function To_Big_Unsigned(S : String) return Big_Unsigned is
      Fence_Count: Natural := 0;
      Temp : Unbounded_String := Null_Unbounded_String;
      M_B  : Natural:=0;
   begin
      if S'Length = 0 then
         raise Conversion_Error;
      else
         for I in reverse S'Range loop
            case S(I) is
               when '0' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,0);
               when '1' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,1);
               when '2' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,2);
               when '3' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,3);
               when '4' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,4);
               when '5' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,5);
               when '6' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,6);
               when '7' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,7);
               when '8' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,8);
               when '9' => Temp:= S(I) & Temp;  M_B:=Natural'Max(M_B,9);
               when 'a' | 'A' => Temp:= 'A' & Temp; M_B:=Natural'Max(M_B,11);
               when 'b' | 'B' => Temp:= 'B' & Temp; M_B:=Natural'Max(M_B,12);
               when 'c' | 'C' => Temp:= 'C' & Temp; M_B:=Natural'Max(M_B,13);
               when 'd' | 'D' => Temp:= 'D' & Temp; M_B:=Natural'Max(M_B,14);
               when 'e' | 'E' => Temp:= 'E' & Temp; M_B:=Natural'Max(M_B,15);
               when 'f' | 'F' => Temp:= 'F' & Temp; M_B:=Natural'Max(M_B,16);
               when '_' | ' ' => null;
               when '#'    => Fence_Count := Fence_Count+1; Temp:= S(I) & Temp;
               when others => raise  Conversion_Error;
            end case;
         end loop;
      end if;

      declare
         Result : Big_Unsigned;
         S2 : constant String := To_String(Temp);
      begin

         -- Base = 10
         if Fence_Count = 0 then
            if M_B > 10 then
               raise Conversion_Error;
            end if;
            for I in  S2'Range loop
               Result := Result * 10 + Get_Digit(S2(I));
            end loop;
            return Result;

            -- Base /= 10
            -- check fences and size (Min_Size=|2#0#|=4)
         elsif Fence_Count /= 2 or S2(S2'Last) /= '#' or  S2(S2'First) = '#'
           or  S2'Length < 4  then
            raise Conversion_Error;
         end if;

         declare
            Base : Number_Base;
         begin
            --Compute and check Base
            if S2(S2'First+1) /= '#' then
               if S2(S2'First+2) /= '#' then
                  raise Conversion_Error;
               end if;
               Base :=  Number_Base(Get_Digit(S2(S2'First)) * 10
                 + Get_Digit(S2(S2'First+1)));
            else Base :=  Number_Base(Get_Digit(S2(S2'First)));
            end if;

            -- Check if all Characters are valid to the base
            if M_B > Base then
               raise Conversion_Error;
            end if;

            --Time to compute the Big_Unsigned
            if Base > 10 then
               for I in S2'First+3..S2'Last-1 loop
                 Result := Result * Word(Base) + Get_Digit(S2(I));
               end loop;
            else
               for I in S2'First+2..S2'Last-1 loop
                  Result := Result * Word(Base) + Get_Digit(S2(I));
               end loop;
            end if;
            return Result;
         end;
      end;
   end To_Big_Unsigned;

   ---------------------------------------------------------------------------

  end Utils;

