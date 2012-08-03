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
-- exception does not however invalidate any other reasons why tthe
-- executable file might be covered by the GNU Public License.

--with Ada.Text_IO; use Ada.Text_IO;

package body  Crypto.Types.Elliptic_Curves.Zp is
   use Big.Mod_Utils; 

   -- y^2 = x^3 + ax + b mod p
   A : Big_Unsigned;
   B : Big_Unsigned;

   -- Prime that creates the finit field Z_p
   P : Big_Unsigned;

   ---------------------------------------------------------------------------

   use Big.Utils;

   procedure Init(Len : Positive ) is
      R : Big_Unsigned;
   begin
      if Len < 3  then
         raise Constraint_Error;
      else
         P := Get_N_Bit_Prime(Len);
         loop
            R := Get_Random(P);
            exit when  R /= Big_Unsigned_Zero and
              Add(Mult(R, Big_Unsigned_Four,P), Big_Unsigned_Zero+27 ,P) /=
              Big_Unsigned_Zero;
         end loop;

         A := Get_Random(P);
         B := Get_Random(P);
         loop
            B := Add(A,Big_Unsigned_One,P);

            exit when
              not(A = Big_Unsigned_Zero and B = Big_Unsigned_Zero) and
              Mult(R,Pow(B,Big_Unsigned_Two,P),P)=Pow(A, Big_Unsigned_Three,P);
         end loop;
      end if;
   end Init;

   ---------------------------------------------------------------------------

   -- init a ec over Z_p
   procedure Init(A, B, P : in Big_Unsigned) is
   begin
      Zp.A := A;
      Zp.B := B;
      Zp.P := P;
   end init ;


   ---------------------------------------------------------------------------

   -- init a ec over Z_p
   procedure Init(ECZ : in Elliptic_Curve_Zp) is
   begin
      Zp.A := ECZ.A;
      Zp.B := ECZ.B;
      Zp.P := ECZ.P;
   end init ;


   ---------------------------------------------------------------------------

   -- test if y^2 = x^3 + ax + b mod p is a valid elliptic curve
   -- functionality:
   -- 1. Test if P is a prim > 3
   -- 2. compute the discriminante  D of the elliptic curve
   -- 3. Test if D /= 0
   function Is_Elliptic_Curve return Boolean  is
      D : Big_Unsigned := Big_Unsigned_Zero  + 27;
   begin
      -- Test 1
      if not (Is_Prime(P) and P > 3) then
         return False;
      else
         -- compute D := -16(4(a**3) + 27(b**2)) mod p
         D := Mult(D,Pow(B,Big_Unsigned_Two,P),P); -- D := 27(b**2)

         -- compute D := D + 4(a**3);
         D := Add(D,Mult(Big_Unsigned_Four, Pow(A,Big_Unsigned_Three,P),P),P);

         -- cumpute D := D * (-16)
         D := Mult(D,Sub(P,Big_Unsigned_Sixteen,P),P);

         if D /= Big_Unsigned_Zero then
            return True;
         else
              return False;
         end if;
      end if;
   end  Is_Elliptic_Curve;

   ---------------------------------------------------------------------------

   function On_Elliptic_Curve(X : EC_Point) return Boolean is
   begin
      if X = EC_Point_Infinity then
         return True;
      end if;

      if Pow(X.Y,Big_Unsigned_Two,P) = Add(Add(Pow(X.X,Big_Unsigned_Three,P),
                                               Mult(A,X.X,P),P),B,P) then
         return True;
      else
         return False;
      end if;
   end On_Elliptic_Curve;

   ---------------------------------------------------------------------------

   function Negative(X : EC_Point) return EC_Point is
      R : EC_Point := X;
   begin
      R.Y := Sub(P,R.Y,P);
      return R;
   end Negative;

   ---------------------------------------------------------------------------

   function Is_Negative(X : EC_Point) return Boolean is
      R : EC_Point := X;
   begin
		if X = EC_Point_Infinity then
			return true;
		end if; 	

      R.Y := Sub(P,R.Y,P);
		if R.Y = Big_Unsigned_Zero then
      	return true;
		else 
      	return false;
		end if;
   end Is_Negative;

   ---------------------------------------------------------------------------

   function Is_Negative(Left, Right : EC_Point) return Boolean is
      R : EC_Point := Right;
   begin
		if Left = EC_Point_Infinity xor Right = EC_Point_Infinity then
			return false;
		end if; 	

		if Left = EC_Point_Infinity and Right = EC_Point_Infinity then
			return true;
		end if;

      R.Y := Sub(P,R.Y,P);
		if R.Y = Left.Y then
      	return true;
		else 
      	return false;
		end if;
   end Is_Negative;

   ---------------------------------------------------------------------------


   function "+"(Left, Right : EC_Point) return EC_Point is
	begin
		
		if Left = EC_Point_Infinity then
			return Right;
		end if; 	

		if Right = EC_Point_Infinity then
			return Left;
		end if;
 	
		if Left = Right then
			return Double(Left);
		end if;

		if Is_Negative(Left, Right) then --Left = Negative(Right) then
			return  EC_Point_Infinity;
		end if;
	
      declare
         Result : EC_Point;
         Temp : constant Big_Unsigned := 
	   Div(Sub(Right.Y,Left.Y,P),Sub(Right.X,Left.X,P),P);
      begin
         Result.X := Sub(Mult(Temp,Temp,P),Left.X, P);
         Result.X := Sub(Result.X, Right.X, P);

         Result.Y := Sub(Mult(Temp, Sub(Left.X, Result.X,P),P),Left.Y,P);
         return Result;
      end;
	end "+";

   ---------------------------------------------------------------------------

   function "-"(Left, Right : EC_Point) return EC_Point is
   begin
      return Left + Negative(Right);
   end; pragma Inline("-");

   ---------------------------------------------------------------------------

   function Double(X : EC_Point) return EC_Point is
   begin
      if Is_Negative(X) then --X = Negative(X) then
         return  EC_Point_Infinity;
		end if;
         declare
            Result : EC_Point;
            Temp : constant Big_Unsigned := 
	      Div(Add(Mult(Mult(X.X,X.X,P),Big_Unsigned_Three,P),A,P),
		  Mult(X.Y,Big_Unsigned_two,P),P);
         begin
            Result.X := Mult(Temp,Temp,P);
            Result.X := Sub(Result.X, Mult(X.X,Big_Unsigned_Two,P),P);
            Result.Y := Sub(Mult(Temp, Sub(X.X,Result.X,P),P),X.Y,P);

            return Result;
         end;
 
  end Double;

   ---------------------------------------------------------------------------

   function "*"(Left : Big_Unsigned; Right : EC_Point) return EC_Point is
      W	: EC_Point;
      M : EC_Point  := Right;
      K : constant Mod_Types :=  Big.Utils.To_Mod_Types(Left);
-------------------------------------------------------------------------
--      Q				: EC_Point;
--      P 				: EC_Point  := Right;
--		tmp_BU 			: Big_Unsigned := Big_Unsigned_One;
--		tmp_BU_count_2	: Big_Unsigned := Big_Unsigned_Zero;
--		tmp_BU_count_3	: Big_Unsigned := Big_Unsigned_Zero;
--		EC_Map 			: Elliptic_Curve_Map.Map;
--		ec_ex				: exception;
   begin
      if Left = Big_Unsigned_Zero then
         return EC_Point_Infinity;
      end if;

      for I in K'First..K'Last-1 loop
         for J in 0..Mod_Type'Size-1 loop
            if (Shift_Right(K(I),J) and 1) = 1  then
               W := W + M;
            end if;
            M := Double(M);
         end loop;
      end loop;


      declare
         L : Natural := 0;
         Y : constant  Mod_Type := K(K'Last);
      begin
         for I  in reverse 0..Mod_Type'Size-1 loop
            if  (Shift_Right(Y,I) and 1) = 1  then
               L := I;
              exit;
            end if;
         end loop;
         for I in 0..L loop
            if (Shift_Right(Y,I) and 1) = 1  then
               W :=  W + M;
            end if;
            M := Double(M);
         end loop;
      end;


------------------------------------------
-- 		Alternativ
------------------------------------------


--		while tmp_BU <= Left loop
--			EC_Map.Insert(tmp_BU, P);			
--			tmp_BU := tmp_BU * Big_Unsigned_Two;
--			P := Double(P);
--		end loop;

--		while Left /= tmp_BU_count_2 loop
--			tmp_BU := tmp_BU / Big_Unsigned_Two;
--			tmp_BU_count_3	:= tmp_BU_count_2 + tmp_BU;
--			if tmp_BU_count_3 <= Left then
--				tmp_BU_count_2 := tmp_BU_count_2 + tmp_BU;
--				Q := Q + EC_Map.Element(tmp_BU);
--			end if;
--			tmp_BU_count_3	:= tmp_BU_count_2;
--		end loop; 

--		if Q /= W then
--			raise ec_ex with "schmuddel";
--		end if; 

      return W;
   end "*";

   ---------------------------------------------------------------------------
   
   function Get_P(ECZ : in Elliptic_Curve_Zp) return  Big_Unsigned is
   begin
      return ECZ.P;
   end Get_P;
  
   
   ---------------------------------------------------------------------------

end Crypto.Types.Elliptic_Curves.Zp;
