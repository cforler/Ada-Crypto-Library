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

with Ada.Unchecked_Conversion;

--pragma Elaborate_All(Generic_Mod_Aux);
  
package body Crypto.Types is
   
   package body Generic_Mod_Aux is
      function "xor"(Left, Right : T_A) return T_A is
	 Result : T_A(0..Left'Length-1);
   begin
      if Left'Length /= Right'Length then
         raise  Constraint_Error;
      end if;
      for I in 0..Left'Length-1 loop
         Result(I) := Left(Left'First+I) xor Right(Right'First+I);
      end loop;
      return Result;
   end "xor";
    
   ------------------------------------------------------------------------
   
   function "xor"(Left : T_A; Right : T) return T_A is
      Result : T_A := Left;
   begin
      Result(Result'Last) := Left(Result'Last) xor Right;
      return Result;
   end "xor";
   
   ------------------------------------------------------------------------

    function "xor"(Left : T; Right : T_A) return T_A is
   begin
      return Right xor Left;
   end "xor";
   
      
      ------------------------------------------------------------------------
      
   function "and"(Left, Right : T_A) return T_A is
	Result : T_A(0..Left'Length-1);
   begin

      if Left'Length /= Right'Length then
         raise  Constraint_Error;
      end if;
      for I in 0..Left'Length-1 loop
         Result(I) := Left(Left'First+I) and Right(Right'First+I);
      end loop;
      return Result;
   end "and";
      
   ------------------------------------------------------------------------
   function "+"(Left : T_A; Right : T) return T_A is
      Result: T_A(Left'Range) := Left;
   begin
      Result(Left'Last) := Left(Left'Last) + Right;

      -- overflow?
      if Result(Left'Last) < Left(Left'Last) then
         for I in reverse Left'First..Left'Last-1 loop
            Result(I):=Result(I)+1;
            if Result(I) /= 0 then
               return  Result;
            end if;
         end loop;
      end if;
      return Result;
   end "+";

   
   ------------------------------------------------------------------------
   function "+"(Left : T; Right : T_A) return T_A is
   begin
      return Right + Left;
   end "+";
   ------------------------------------------------------------------------
   
   function Is_Zero(Item : T_A) return Boolean is
   begin
      for I in  Item'Range loop
         if Item(I) /= 0 then return False;
         end if;
      end loop;
      return True;
   end Is_Zero;
   
------------------------------------------------------------------------
   
   function Left_Part(Block : in T_A) return T_A is
      Len  : constant Natural := ((Block'Length+1)/2)-1;
      Left : constant T_A(0..Len) := Block(Block'First..(Block'First+Len));
   begin
      return Left;
   end Left_Part;
   
   ------------------------------------------------------------------------
   
   function Right_Part(Block : in T_A) return T_A is
      Len : constant Natural := Block'Length/2; 
      Right : constant T_A(0..Len-1) := Block(Block'Last-Len+1..Block'Last);
   begin
      return Right;
   end Right_Part;
   
   ------------------------------------------------------------------------
   
   function Shift_Left(Value : T_A;  Amount : Natural) return T_A  is
      Result : T_A(Value'Range) := (others => 0);
      L : constant Natural := Amount mod T'Size;
      M : constant Natural := Value'First+(Amount/T'Size);
   begin
      if Amount >= Value'Size then
	 return Result;
      elsif Amount = 0 then
	 return Value;
      end if;      
      Result(Value'Last-M) := Shift_Left(Value(Value'Last),L);      
      
      for I in reverse Value'First..Value'Last-(M+1) loop
	 Result(I) := Shift_Left(Value(I),L)
	   xor Shift_Right(Value(I+1),T'Size-L);
      end loop;
      return Result;
      end Shift_Left; 
      -------------------------------------------------------------------------
         function Shift_Right(Value : T_A;  Amount : Natural) return T_A  is
      Result : T_A(Value'Range) := (others => 0);
      L : constant Natural := Amount mod T'Size;
      M : constant Natural := Value'First+(Amount/T'Size);
   begin
      if Amount >= Value'Size then
	 return Result;
      elsif Amount = 0 then
	 return Value;
      end if;      
      Result(Value'Last-M) := Shift_Right(Value(Value'Last),L);      
      
      for I in reverse Value'First..Value'Last-(M+1) loop
	 Result(I) := Shift_Right(Value(I),L)
	   xor Shift_Left(Value(I+1),T'Size-L);
      end loop;
      return Result;
   end Shift_Right;    

   end Generic_Mod_Aux;
   
   
   function Cast  is new Ada.Unchecked_Conversion (Byte_Word, Word);
   function Cast  is new Ada.Unchecked_Conversion (Word, Byte_Word);
   function DCast is new Ada.Unchecked_Conversion (Byte_DWord, DWord);
   function DCast is new Ada.Unchecked_Conversion (DWord, Byte_DWord);
   pragma Inline (Cast, DCast);
  
      
   package Aux_Byte is new Generic_Mod_Aux(Byte,Bytes);
   package Aux_Word is new Generic_Mod_Aux(Word,Words);
   package Aux_DWord is new Generic_Mod_Aux(Dword,DWords);
   
   ---------------------------------------------------------------------------

   function To_Word(A,B,C,D : Character) return Word is
   begin
      return Cast((Byte(Character'Pos(D)), Byte(Character'Pos(C)),
                   Byte(Character'Pos(B)), Byte(Character'Pos(A))));
   end To_Word;

   ---------------------------------------------------------------------------

   function To_Word(A,B,C,D : Byte) return Word is
   begin
      return Cast((D, C, B, A));
   end To_Word;

   ---------------------------------------------------------------------------

   function To_Word (X : Byte_Word) return Word is
   begin
      return Cast((X(3), X(2), X(1), X(0)));
   end To_Word;

   ---------------------------------------------------------------------------

   function R_To_Word (X : Byte_Word) return Word is
   begin
      return Cast(X);
   end R_To_Word;

   ---------------------------------------------------------------------------

   function To_Bytes (X : Word) return Byte_Word is
   begin
      return (Cast(X)(3), Cast(X)(2), Cast(X)(1), Cast(X)(0));
   end To_Bytes;

   ---------------------------------------------------------------------------

   function R_To_Bytes (X : Word) return Byte_Word is
   begin
      return Cast(X);
   end R_To_Bytes;

   ---------------------------------------------------------------------------

   function Byte0 (W : Word) return Byte is
   begin
      return Cast(W)(3);
   end Byte0;

   ---------------------------------------------------------------------------

   function Byte1 (W : Word) return Byte is
   begin
      return Cast(W)(2);
   end Byte1;

   ---------------------------------------------------------------------------

   function Byte2 (W : Word) return Byte is
   begin
      return Cast(W)(1);
   end Byte2;

   ---------------------------------------------------------------------------

   function Byte3 (W : Word) return Byte is
   begin
      return Cast(W)(0);
   end Byte3;

   ---------------------------------------------------------------------------

   function To_DWord (X : Byte_DWord) return DWord is
   begin
      return DCast((X(7), X(6), X(5), X(4), X(3), X(2), X(1), X(0)));
   end To_DWord;

   ---------------------------------------------------------------------------

   function R_To_DWord (X : Byte_DWord) return DWord is
   begin
      return DCast(X);
   end R_To_DWord;

   ---------------------------------------------------------------------------

   function To_Bytes (X : DWord) return Byte_DWord is
   begin
      return (DCast(X)(7), DCast(X)(6), DCast(X)(5), DCast(X)(4),
              DCast(X)(3), DCast(X)(2), DCast(X)(1), DCast(X)(0));

   end To_Bytes;

   ---------------------------------------------------------------------------

   function R_To_Bytes (X : DWord) return Byte_DWord is
   begin
      return DCast(X);
   end R_To_Bytes;

   ---------------------------------------------------------------------------

   function Byte0 (D : DWord) return Byte is
   begin
      return DCast(D)(7);
   end Byte0;

   ---------------------------------------------------------------------------
   function Byte1 (D : DWord) return Byte is
   begin
      return DCast(D)(6);
   end Byte1;

   ---------------------------------------------------------------------------

   function Byte2 (D : DWord) return Byte is
   begin
      return DCast(D)(5);
   end Byte2;

   ---------------------------------------------------------------------------

    function Byte3 (D : DWord) return Byte is
   begin
      return DCast(D)(4);
   end Byte3;

   ---------------------------------------------------------------------------

    function Byte4 (D : DWord) return Byte is
   begin
      return DCast(D)(3);
   end Byte4;

   ---------------------------------------------------------------------------

    function Byte5 (D : DWord) return Byte is
   begin
      return DCast(D)(2);
   end Byte5;

   ---------------------------------------------------------------------------

    function Byte6 (D : DWord) return Byte is
   begin
      return DCast(D)(1);
   end Byte6;

   ---------------------------------------------------------------------------

   function Byte7 (D : DWord) return Byte is
   begin
      return DCast(D)(0);
   end Byte7;

   ---------------------------------------------------------------------------


   function "xor"(Left, Right : Bytes) return Bytes is
   begin
      return Aux_Byte."xor"(Left,Right);
   end "xor";

   ---------------------------------------------------------------------------
   
   function "xor"(Left : Bytes; Right : Byte) return Bytes is
   begin
      return Aux_Byte."xor"(Left,Right);
   end "xor";
   
   ---------------------------------------------------------------------------
      
   function "+"(Left : Bytes; Right : Byte) return Bytes is
   begin
      return Aux_Byte."+"(Left,Right);
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : Byte; Right : Bytes) return Bytes is
   begin
      return Right + Left;
   end "+";
   
   ---------------------------------------------------------------------------
   
   function "and"(Left, Right : Bytes) return Bytes is
   begin
      return Aux_Byte."and"(Left,Right);
   end "and";

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function "xor"(Left, Right : Words) return Words is
   begin
      return Aux_Word."xor"(Left,Right);
   end "xor";

   ---------------------------------------------------------------------------

   function "+"(Left : Words; Right : Word) return Words is

   begin
      return Aux_Word."+"(Left,Right);
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : Word; Right : Words) return Words is
   begin
      return Right + Left;
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : Words; Right : Byte) return Words is
    Result: Words(Left'Range) := Left;
   begin
      Result(Left'Last) := Left(Left'Last) + Word(Right);

      -- overflow?
      if Result(Left'Last) < Left(Left'Last) then
         for I in reverse Left'First..Left'Last-1 loop
            Result(I):=Result(I)+1;
            if Result(I) /= 0 then
               return  Result;
            end if;
         end loop;
      end if;
      return Result;
   end "+";

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function "xor"(Left, Right : DWords) return DWords is
   begin
      return Aux_DWord."xor"(Left,Right);
   end "xor";

   ---------------------------------------------------------------------------

   function "+"(Left : DWords; Right : DWord) return DWords is
   begin
      return Aux_DWord."+"(Left,Right);
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : DWord; Right : DWords) return DWords is
   begin
      return Right + Left;
   end "+";

   ---------------------------------------------------------------------------

   function "+"(Left : DWords; Right : Byte) return DWords is
    Result: DWords(Left'Range) := Left;
   begin
      Result(Left'Last) := Left(Left'Last) + DWord(Right);

      -- overflow?
      if Result(Left'Last) < Left(Left'Last) then
         for I in reverse Left'First..Left'Last-1 loop
            Result(I):=Result(I)+1;
            if Result(I) /= 0 then
               return  Result;
            end if;
         end loop;
      end if;
      return Result;
   end "+";

   ---------------------------------------------------------------------------

   function To_Words(Byte_Array : Bytes) return Words is
      L : constant Natural :=
        Natural(Float'Ceiling(Float(Byte_Array'Length)/4.0))-1;
      W : Words(0..L) := (others => 0);
      N : Integer := Byte_Array'First;
      S : Natural :=24;
   begin

      for I in 0..(Byte_Array'Length/4)-1 loop
         W(I) := To_Word(Byte_Array(N..N+3));
         N := N+4;
      end loop;

      for I in  1..(Byte_Array'Length mod 4) loop
         W(L):= W(L) or Shift_Left(Word(Byte_Array(N)),S);
         N := N+1;
         S := S-8;
      end loop;

      return W;
   end To_Words;

   ---------------------------------------------------------------------------

   function To_Bytes(Word_Array : Words) return Bytes is
      B : Bytes(1..Word_Array'Length*4);
      C : Natural := 1;
   begin
      for I in  Word_Array'Range loop
         B(C..C+3) := To_Bytes(Word_Array(I));
         C:=C+4;
      end loop;
      return B;
   end To_Bytes;

   ---------------------------------------------------------------------------

   function To_DWords(Byte_Array : Bytes) return DWords is
      L : constant Natural := 
	Natural(Float'Ceiling(Float(Byte_Array'Length)/8.0))-1;
      W : DWords(0..L):=(others => 0);
      N : Natural := Byte_Array'First;
      S : Natural := 56;
   begin

      for I in 0..(Byte_Array'Length/8)-1 loop
         W(I) := To_DWord(Byte_Array(N..N+7));
         N := N+8;
      end loop;

      for I in  1..(Byte_Array'Length mod 8) loop
         W(L):= W(L) or Shift_Left(DWord(Byte_Array(N)),S);
         N := N+1;
         S := S-8;
      end loop;

      return W;
   end To_DWords;

   ---------------------------------------------------------------------------


    function To_Bytes(DWord_Array : DWords) return Bytes is
      B : Bytes(1..DWord_Array'Length*8);
      C : Natural := 1;
   begin
      for I in  DWord_Array'Range loop
         B(C..C+7) := To_Bytes(DWord_Array(I));
         C:=C+8;
      end loop;
      return B;
   end To_Bytes;
   
   ---------------------------------------------------------------------------

   function To_Hex(B : Byte) return Hex_Byte is
      S : constant String := "0123456789ABCDEF";
      H : Hex_Byte;
   begin
      H(2) := S(Natural(B and 15)+1);
      H(1) := S(Natural(Shift_Right(B,4)+1));
      return H;
   end To_Hex;

   ---------------------------------------------------------------------------

   function To_Hex(W : Word) return Hex_Word is
      S : constant String := "0123456789ABCDEF";
      H : Hex_Word;
      T : Word := W;
   begin
      for I in reverse  H'Range loop
        H(I) :=  S(Natural(T and 15)+1);
        T := Shift_Right(T,4);
      end loop;
      return H;
   end To_Hex;

   ---------------------------------------------------------------------------


   function To_Hex(D : DWord) return Hex_DWord is
      S : constant String := "0123456789ABCDEF";
      H : Hex_DWord;
      T : DWord := D;
   begin
      for I in reverse H'Range loop
         H(I) :=  S(Natural(T and 15)+1);
         T := Shift_Right(T,4);
      end loop;

      return H;
   end To_Hex;


   function Hex_To_Bytes(Hex : String) return Bytes is
    Return_Bytes : Bytes(0..Hex'Length/2 -1);
   begin
   for I in Return_Bytes'Range loop
      case Hex(Hex'First + 2*i) is
         when '0'  => Return_Bytes(i):=16#00#;
         when '1'  => Return_Bytes(i):=16#01#;
         when '2' => Return_Bytes(i):=16#02#;
         when '3' => Return_Bytes(i):=16#03#;
         when '4' => Return_Bytes(i):=16#04#;
         when '5' => Return_Bytes(i):=16#05#;
         when '6' => Return_Bytes(i):=16#06#;
         when '7' => Return_Bytes(i):=16#07#;
         when '8' => Return_Bytes(i):=16#08#;
         when '9' => Return_Bytes(i):=16#09#;
         when 'A' => Return_Bytes(i):=16#0A#;
         when 'B' => Return_Bytes(i):=16#0B#;
         when 'C' => Return_Bytes(i):=16#0C#;
         when 'D' => Return_Bytes(i):=16#0D#;
         when 'E' => Return_Bytes(i):=16#0E#;
         when 'F' => Return_Bytes(i):=16#0F#;
         when others => null;
      end case;
      
      case Hex(Hex'First + 2*i + 1) is
         when '0' => Return_Bytes(i):= Return_Bytes(i) + 16#00#;
         when '1' => Return_Bytes(i):= Return_Bytes(i) + 16#10#;
         when '2' => Return_Bytes(i):= Return_Bytes(i) + 16#20#;
         when '3' => Return_Bytes(i):= Return_Bytes(i) + 16#30#;
         when '4' => Return_Bytes(i):= Return_Bytes(i) + 16#40#;
         when '5' => Return_Bytes(i):= Return_Bytes(i) + 16#50#;
         when '6' => Return_Bytes(i):= Return_Bytes(i) + 16#60#;
         when '7' => Return_Bytes(i):= Return_Bytes(i) + 16#70#;
         when '8' => Return_Bytes(i):= Return_Bytes(i) + 16#80#;
         when '9' => Return_Bytes(i):= Return_Bytes(i) + 16#90#;
         when 'A' => Return_Bytes(i):= Return_Bytes(i) + 16#A0#;
         when 'B' => Return_Bytes(i):= Return_Bytes(i) + 16#B0#;
         when 'C' => Return_Bytes(i):= Return_Bytes(i) + 16#C0#;
         when 'D' => Return_Bytes(i):= Return_Bytes(i) + 16#D0#;
         when 'E' => Return_Bytes(i):= Return_Bytes(i) + 16#E0#;
         when 'F' => Return_Bytes(i):= Return_Bytes(i) + 16#F0#;
         when others => null;
      end case;
      
   end loop;
      return Return_Bytes;
   end Hex_To_Bytes;

   ---------------------------------------------------------------------------

   function Is_Zero(Byte_Array : Bytes) return Boolean is
   begin
      return Aux_Byte.Is_Zero(Byte_Array);
   end Is_Zero;

   ---------------------------------------------------------------------------


   function Is_Zero(Word_Array : Words) return Boolean is
   begin
      return Aux_Word.Is_Zero(Word_Array);
   end Is_Zero;

   ---------------------------------------------------------------------------

   function Is_Zero(DWord_Array : Dwords) return Boolean is
   begin
      return Aux_DWord.Is_Zero(DWord_Array);
   end Is_Zero;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function To_Bytes(Message : String) return Bytes is
     B : Bytes(Message'Range);
   begin
      for I in Message'Range loop
         B(I) := Character'Pos(Message(I));
      end loop;
      return B;
   end To_Bytes;

   ---------------------------------------------------------------------------

   function To_String(ASCII : Bytes) return String is
      S : String(1..ASCII'Length);
      J : Integer:=1;
   begin
      for I in ASCII'Range loop
         S(J) := Character'Val(ASCII(I));
         J:=J+1;
      end loop;
      return S;
   end To_String;
   
   ---------------------------------------------------------------------------
   
   function Left_Part(Block : in Bytes) return Bytes is
   begin
      return Aux_Byte.Left_Part(Block);
   end Left_Part;
   
   ---------------------------------------------------------------------------
   
   function Right_Part(Block : in Bytes) return Bytes is
   begin
      return Aux_Byte.Right_Part(Block);
   end Right_Part;
   
   ---------------------------------------------------------------------------
   
    function To_Bytes(B : B_Block64) return Bytes is
   begin
      return Bytes(B);
   end To_Bytes;
    
   ---------------------------------------------------------------------------
   
   function To_Bytes(B : B_Block128) return Bytes is
   begin
      return Bytes(B);
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(B : B_Block192) return Bytes is
      
   begin
      return Bytes(B);
   end To_Bytes;
   
    ---------------------------------------------------------------------------
   
   function To_Bytes(B : B_Block256) return Bytes is
   begin
      return Bytes(B);
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(W : W_Block160) return Bytes is
   begin
      return To_Bytes(Words(W));
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
    function To_Bytes(W : W_Block256) return Bytes is
   begin
      return To_Bytes(Words(W));
   end To_Bytes;
   
   ---------------------------------------------------------------------------
      
    function To_Bytes(W : W_Block512) return Bytes is
   begin
      return To_Bytes(Words(W));
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(D : DW_Block256) return Bytes is
   begin
      return To_Bytes(DWords(D));
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(D : DW_Block384) return Bytes is
   begin
      return To_Bytes(DWords(D));
   end To_Bytes;
   
   ---------------------------------------------------------------------------
   
   function To_Bytes(D : DW_Block512) return Bytes is
   begin
      return To_Bytes(DWords(D));
   end To_Bytes;

   ---------------------------------------------------------------------------
   
   function To_Bytes(D : DW_Block1024) return Bytes is
   begin
      return To_Bytes(DWords(D));
   end To_Bytes;

   ---------------------------------------------------------------------------
   
   function To_Bytes(D : DW_Block8192) return Bytes is
   begin
      return To_Bytes(DWords(D));
   end To_Bytes;
   
   ---------------------------------------------------------------------------  
   
   function To_B_Block64(B : Bytes) return B_Block64 is
   begin
      return B_Block64(B);
   end To_B_Block64;
   
   ---------------------------------------------------------------------------
   
   function To_B_Block128(B : Bytes) return B_Block128 is
   begin
      return B_Block128(B);
   end To_B_Block128;
      
   ---------------------------------------------------------------------------
      
   function To_B_Block192(B : Bytes) return B_Block192 is
   begin
      return B_Block192(B);
   end To_B_Block192;
   
      ---------------------------------------------------------------------------
      
   function To_B_Block256(B : Bytes) return B_Block256 is
   begin
      return B_Block256(B);
   end To_B_Block256;
   
   ---------------------------------------------------------------------------
   
   function To_W_Block160(B : Bytes) return W_Block160 is
   begin
     return W_Block160(To_Words(B));
   end To_W_Block160;
   
    ---------------------------------------------------------------------------
   
   function To_W_Block256(B : Bytes) return W_Block256 is
   begin
     return W_Block256(To_Words(B));
   end To_W_Block256;
   
    ---------------------------------------------------------------------------
   
   function To_W_Block512(B : Bytes) return W_Block512 is
   begin
     return W_Block512(To_Words(B));
   end To_W_Block512;
   
   ---------------------------------------------------------------------------
      
   function To_DW_Block256(B : Bytes) return DW_Block256 is
   begin
     return DW_Block256(To_DWords(B));
   end To_DW_Block256;
   
   ---------------------------------------------------------------------------
   
   function To_DW_Block384(B : Bytes) return DW_Block384 is
   begin
     return DW_Block384(To_DWords(B));
   end To_DW_Block384;
   
   ---------------------------------------------------------------------------
     
   function To_DW_Block512(B : Bytes) return DW_Block512 is
   begin
     return DW_Block512(To_DWords(B));
   end To_DW_Block512;
      
   ---------------------------------------------------------------------------
     
   function To_DW_Block1024(B : Bytes) return DW_Block1024 is
   begin
     return DW_Block1024(To_DWords(B));
   end To_DW_Block1024;

   ---------------------------------------------------------------------------
     
   function To_DW_Block8192(B : Bytes) return DW_Block8192 is
   begin
     return DW_Block8192(To_DWords(B));
   end To_DW_Block8192;
   
   ---------------------------------------------------------------------------
   
   function "xor"(Left, Right : W_Block160) return  W_Block160 is
   begin
      return W_Block160(Words(Left) xor Words(Right));
   end "xor";
   ---------------------------------------------------------------------------
   
   function "xor"(Left, Right : W_Block256) return  W_Block256 is
   begin
      return W_Block256(Words(Left) xor Words(Right));
   end "xor";
   ---------------------------------------------------------------------------
   
   function "xor"(Left, Right : W_Block512) return  W_Block512 is
   begin
      return W_Block512(Words(Left) xor Words(Right));
   end "xor";
   
   ---------------------------------------------------------------------------

   function "xor"(Left, Right : DW_Block512) return  DW_Block512 is
   begin
      return DW_Block512(DWords(Left) xor DWords(Right));
   end "xor";
   
   ---------------------------------------------------------------------------
     
   function "xor"(Left, Right : DW_Block1024) return  DW_Block1024 is
   begin
      return DW_Block1024(DWords(Left) xor DWords(Right));
   end "xor"; 

   ---------------------------------------------------------------------------
     
   function "xor"(Left, Right : DW_Block8192) return  DW_Block8192 is
   begin
      return DW_Block8192(DWords(Left) xor DWords(Right));
   end "xor"; 
   
   ---------------------------------------------------------------------------
   
   function "xor"(Left, Right : B_Block128) return  B_Block128 is
   begin
      return B_Block128(Bytes(Left) xor Bytes(Right));
   end "xor"; 
     
   ---------------------------------------------------------------------------
   
   function "xor"(Left, Right : B_Block64) return  B_Block64 is
   begin
      return B_Block64(Bytes(Left) xor Bytes(Right));
   end "xor"; 
   
   ---------------------------------------------------------------------------
   
   function "+"(Left : B_Block128; Right : Byte) return B_Block128 is
   begin
      return B_Block128(Bytes(Left) + Right);
   end "+";
   
   ---------------------------------------------------------------------------
   
   function Shift_Left(Value : Bytes; Amount : Natural) return Bytes is
   begin
      return Aux_Byte.Shift_Left(Value,Amount);
   end Shift_Left;
   
   ---------------------------------------------------------------------------
   
   function Shift_Left(Value : B_Block128; Amount :Natural) return B_Block128 is
   begin
      return  B_Block128(Aux_Byte.Shift_Left(Bytes(Value),Amount));
   end Shift_Left;
   
   ----------------------------------------------------------------------------
   
   function Shift_Right(Value : Bytes; Amount : Natural) return Bytes is
   begin
      return Aux_Byte.Shift_Right(Value,Amount);
   end Shift_Right;
   
   ---------------------------------------------------------------------------
   
   function Shift_Right(Value : B_Block128; Amount :Natural)return B_Block128 is
   begin
      return  B_Block128(Aux_Byte.Shift_Right(Bytes(Value),Amount));
   end Shift_Right;
   
 
   
   
  end Crypto.Types;
