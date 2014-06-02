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


with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;

package body Crypto.Symmetric.Mode.BPS is
   TL : Bytes(B_Block32'Range);
   TR : Bytes(B_Block32'Range);
   
   ---------------------------------------------------------------------------

   procedure Init(Key : in Key_Type; Initial_Value : in B_Block64) is
   begin
      Set_IV(Initial_Value);
      BC.Prepare_Key(Key);
   end Init;

   ---------------------------------------------------------------------------
   
   procedure Encrypt(Plaintext : in Numerals; Ciphertext : out Numerals) is
      TTL : constant Bytes := TL;
      TTR : constant Bytes := TR; 
      Max_B : Natural;
      Len   : constant Natural :=  Plaintext'Length;
      Rest  : Natural;
   begin
      Max_B := 2*Natural( Log( 2.0**Long_Float(BC.Block'Size-32), Long_Float(Radix) ) );
      Rest := Len mod Max_B;
      if  Len <= Max_B then
	 Ciphertext := BC_Encrypt(Plaintext);
	 return;
      end if;

      declare 
	 I : Natural := 0;
	 C : Natural := 0;
      begin
	 Ciphertext := Plaintext;
	 while Len - C > Max_B loop
	    TL := TTL xor To_Bytes(Word(I*16#10000#));
	    TR := TTR xor To_Bytes(Word(I*16#10000#));
	    
	    if I /= 0 then
	       Ciphertext(C..C+Max_B-1) := 
		 Ciphertext(C-Max_B..C-1) + Ciphertext(C..C+Max_B-1); 
	    end if;
	    Ciphertext(C..C+Max_B-1) := BC_Encrypt(Ciphertext(C..C+Max_B-1));
	    
	    C := C+Max_B;
	    I := I+1;
	 end loop;
	 
	 if Len /= C then
	    TL := TTL xor To_Bytes(Word(I*16#10000#));
	    TR := TTR xor To_Bytes(Word(I*16#10000#));
	    Ciphertext(Len-Rest..Len-1) := 
	      Ciphertext(Len-Rest-Max_B..Len-Max_B-1) +
	      Ciphertext(Len-Rest..Len-1);
	    Ciphertext(Len-Max_B..Len-1) := 
	      Bc_Encrypt(Ciphertext(Len-Max_B..Len-1));
	 end if;
	 TL := TTL;
	 TR := TTR;
      end;     
end Encrypt ;
	
   ---------------------------------------------------------------------------
   
   procedure Decrypt(Ciphertext : in Numerals; Plaintext : out Numerals) is
      TTL : constant Bytes := TL;
      TTR : constant Bytes := TR;  
      Max_B : Natural;
      Len   : constant Natural :=  Ciphertext'Length;
   begin
      Max_B := 2*Natural( Log( 2.0**Long_Float(BC.Block'Size-32), Long_Float(Radix) ) );
      if  Len <= Max_B then
	 Plaintext := BC_Decrypt(Ciphertext);
	 return;
      end if;
      Plaintext := Ciphertext;
      declare
	 Rest : constant Natural := Len mod Max_B;
	 C : Natural := Len-Rest;
	 I : Natural := C/Max_B;
      begin
	 if Len /=C then
	    TL := TTL xor To_Bytes(Word(I*16#10000#));
	    TR := TTR xor To_Bytes(Word(I*16#10000#));
	    Plaintext(Len-Max_B..Len-1) :=
	      Bc_Decrypt( Plaintext(Len-Max_B..Len-1));
	    Plaintext(Len-Rest..Len-1) := Plaintext(Len-Rest..Len-1)-
	      Plaintext(Len-Max_B-Rest..Len-Max_B-1); 
	 end if;
	 while C/=0 loop
	    C := C-Max_B;
	    I:=I-1;
	    TL := TTL xor To_Bytes(Word(I*16#10000#));
	    TR := TTR xor To_Bytes(Word(I*16#10000#));
	    Plaintext(C..C+Max_B-1) :=
	      Bc_Decrypt(Plaintext(C..C+Max_B-1));
	    if(I /=0) then
	       Plaintext(C..C+Max_B-1) := 
		 Plaintext(C..C+Max_B-1) - Plaintext(C-Max_b..C-1); 
	    end if;
	 end loop;	   
      end;
      TL := TTL;
      TR := TTR;
   end Decrypt;
   
   ---------------------------------------------------------------------------
         
   -- length of numerals must be at least 2
   function BC_Encrypt(Plaintext : in Numerals) return  Numerals is
      Ciphertext : Numerals(Plaintext'Range);
      L : constant Natural := ((Plaintext'Length+1)/2);
      R : constant Natural := ((Plaintext'Length)/2);
      Left  : Big_Unsigned := 
	To_Big_Unsigned(Plaintext(Plaintext'First..Plaintext'First+L-1));
      Right : Big_Unsigned := 
	To_Big_Unsigned(Plaintext(Plaintext'First+L..Plaintext'Last));
      Plaintextblock : Bytes(0..BC.Block'Size/8-1) := (others =>0);
      Ciphertextblock : BC.Block;
      SL : constant Big_Unsigned := S**(Big_Unsigned_Zero + Word(L));
      SR : constant Big_Unsigned := S**(Big_Unsigned_Zero + Word(R));
      C : constant Natural := (Plaintextblock'Size)-32;
      Temp : Big_Unsigned;
   begin
      for I in 0..Rounds-1 loop
	 if((I mod 2)=0) then -- Even
	    Temp := Shift_Left(To_Big_Unsigned(TR xor Byte(I)),C)+Right;
	    declare
	       Temp2 : constant  Bytes:= To_Bytes(Temp);
	    begin
	       Plaintextblock(Temp2'Range) := Temp2;
		 BC.Encrypt(BC.To_Block(Plaintextblock),Ciphertextblock);
	       Temp := To_Big_Unsigned(To_Bytes(Ciphertextblock));
	       Left := Add(Left,Temp,SL);
	    end;
	 else --Odd
	    Temp := Shift_Left(To_Big_Unsigned(TL xor Byte(I)),C)+Left;
	    declare
	       Temp2 : constant  Bytes:= To_Bytes(Temp);
	    begin
	       Plaintextblock(Temp2'Range) := Temp2;
	       BC.Encrypt(BC.To_Block(Plaintextblock),Ciphertextblock);
	       Right := Add(Right,To_Big_Unsigned(To_Bytes(Ciphertextblock)),SR);
	    end;
	    end if;
	 end loop;
  
      for I in 0..L-1 loop
	 Temp := Left mod S;
	 Left := (Left - Temp)/S; 
	 Ciphertext(Ciphertext'First+I) := Numeral(To_Bytes(Temp)(0));
      end loop;
      
      for I in 0..R-1 loop
	 Temp := Right mod S;
	 Right := (Right - Temp)/S; 
	 Ciphertext(Ciphertext'First+L+I) := Numeral(To_Bytes(Temp)(0));
      end loop;
      return Ciphertext;
      
   end BC_Encrypt;

   ---------------------------------------------------------------------------

   function BC_Decrypt(Ciphertext : in Numerals) return Numerals is
      Plaintext : Numerals(Ciphertext'Range);
      L : constant Natural := ((Ciphertext'Length+1)/2);
      R : constant Natural := ((Ciphertext'Length)/2);
      Left  : Big_Unsigned := 
	To_Big_Unsigned(Ciphertext(Ciphertext'First..Ciphertext'First+L-1));
      Right : Big_Unsigned := 
	To_Big_Unsigned(Ciphertext(Ciphertext'First+L..Ciphertext'Last));
      Plaintextblock : BC.Block;
      Ciphertextblock : Bytes(0..BC.Block'Size/8-1) := (others =>0);
      SL : constant Big_Unsigned := S**(Big_Unsigned_Zero + Word(L));
      SR : constant Big_Unsigned := S**(Big_Unsigned_Zero + Word(R));
      C : constant Natural :=  (Ciphertextblock'Size)-32;  
      Temp : Big_Unsigned;
   begin
      for I in reverse 0..Rounds-1 loop
	 if((I mod 2)=0) then -- Even
	    Temp := Shift_Left(To_Big_Unsigned(TR xor Byte(I)),C)+Right;
	     declare
		Temp2 : constant  Bytes:= To_Bytes(Temp);
	     begin
		Ciphertextblock(Temp2'Range) := Temp2;
		BC.Encrypt(BC.To_Block(Ciphertextblock),Plaintextblock);
		Left := Sub(Left,To_Big_Unsigned(To_Bytes(Plaintextblock)),SL);
	     end;
	 else --Odd
	    Temp := Shift_Left(To_Big_Unsigned(TL xor Byte(I)),C)+Left;
	    declare
	       Temp2 : constant  Bytes:= To_Bytes(Temp);
	    begin
	       Ciphertextblock(Temp2'Range) := Temp2;
	       BC.Encrypt(BC.To_Block(Ciphertextblock),Plaintextblock);
	       Right := Sub(Right,To_Big_Unsigned(To_Bytes(Plaintextblock)),SR);
	    end;
	 end if;
      end loop;
      
      for I in 0..L-1 loop
	 Temp := Left mod S;
	 Left := (Left - Temp)/S; 
	 Plaintext(Plaintext'First+I) := Numeral(To_Bytes(Temp)(0));
      end loop;
      
      for I in 0..R-1 loop
	 Temp := Right mod S;
	 Right := (Right - Temp)/S; 
	 Plaintext(Plaintext'First+L+I) := Numeral(To_Bytes(Temp)(0));
      end loop;  
      
      return Plaintext;
   end BC_Decrypt;

   ---------------------------------------------------------------------------

    procedure Set_IV(Initial_Value : in B_Block64) is
    begin
       TL := Left_Part(Bytes(Initial_Value));
       TR := Right_Part(Bytes(Initial_Value));	 
    end Set_IV;

    ---------------------------------------------------------------------------
    
    function To_Big_Unsigned(N : Numerals) return Big_Unsigned is
       Result : Big_Unsigned := Big_Unsigned_Zero;
    begin
       for I in 0..N'Length-1 loop
 	  Result := Result +( ( S**(Big_Unsigned_Zero + Word(I)) ) * 
				Word(N(N'First+I)) );
       end loop;
       return Result;
    end To_Big_Unsigned;
   
    ---------------------------------------------------------------------------
    
    function "+"(Left, Right : Numerals) return Numerals is
       Result : Numerals(0..Left'Length-1) := Left;
       Len : Natural;
    begin
       if Left'Length <= Right'Length then
	  Len := Left'Length-1;
       else 
	  Len := Right'Length-1;
       end if;
       for I in 0..Len loop
	  Result(I) := Result(I) + Right(Right'First+I);
       end loop;
	   
       return Result;
    end "+";
        
    ---------------------------------------------------------------------------
    
     function "-"(Left, Right : Numerals) return Numerals is
       Result : Numerals(0..Left'Length-1) := Left;
       Len : Natural;
    begin
       if Left'Length <= Right'Length then
	  Len := Left'Length-1;
       else 
	  Len := Right'Length-1;
       end if;
	  
       for I in 0..Len loop
	  Result(I) := Result(I) - Right(Right'First+I);
       end loop;
       return Result;
    end "-";
    
    ---------------------------------------------------------------------------

end Crypto.Symmetric.Mode.BPS;
