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


package body Crypto.Types.Base64 is
   
   function Encode_Base64(B: Bytes) return Base64_String is
      Len :  constant Natural :=  B'Length / 3;
      Rest : constant Natural :=  B'Length mod 3;
      Result : Base64_String(1..( 4* (((B'Length-1)/3)+1)));
      W : Word;
      J : Natural := B'First;
   begin
      for I in 1..Len loop
	 W := Shift_Left(Word(B(J)),16) or  Shift_Left(Word(B(J+1)),8)
	   or Word(B(J+2));
	 J := J + 3;
	 
	 Result(4*I-3) :=  Base64_Character'Val(Shift_Right(W,18));
	 Result(4*I-2) :=  Base64_Character'Val(Shift_Right(W,12) and 63);
	 Result(4*I-1) :=  Base64_Character'Val(Shift_Right(W,6)  and 63);
	 Result(4*I)   :=  Base64_Character'Val(W  and 63);
      end loop;
      
      if(Rest > 0) then
	 Result(Result'Last) := Base64_Character'Last;
	 
	 if(Rest = 1) then
	    W := Shift_Left(Word(B(B'Last)),16);
	    Result(Result'Last-1) := Base64_Character'Last;
	 else
	    W := Shift_Left(Word(B(B'Last-1)),16) or Shift_Left(Word(B(B'Last)),8);
	    Result(Result'Last-1) :=  Base64_Character'Val(Shift_Right(W,6)  and 63);
	 end if;
	 Result(Result'Last-2) :=   Base64_Character'Val(Shift_Right(W,12) and 63);
	 Result(Result'Last-3) :=   Base64_Character'Val(Shift_Right(W,18));
      end if;
      return Result;
   end Encode_Base64;
end Crypto.Types.Base64;
