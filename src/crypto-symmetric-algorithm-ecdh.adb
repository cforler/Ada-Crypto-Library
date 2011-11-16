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

package body Crypto.Symmetric.Algorithm.ECDH is

   use Big.Mod_Utils;
   use Big.Utils;

-------------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : ECDH_P_KEY) return Boolean is
   begin
      if Key.n /= Big_Unsigned_Zero then
         return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);


-------------------------------------------------------------------------------

   procedure Gen_Public_Key(Public_Key_A  : out Public_Key_ECDH;
			    length      : in DB.Bit_Length) is
   begin
      Get_Elliptic_Curve(Public_Key_A.E, Public_Key_A.P, 
			 Public_Key_A.n, length);
      init(Public_Key_A.E);
      
   end Gen_Public_Key;
   
-------------------------------------------------------------------------------

   procedure Gen_Single_Private_Key(Public_Key_A  : in out Public_Key_ECDH;
				    Private_Key_A : out  Private_Key_ECDH) is
   begin
      Private_Key_A.d := Get_Random(Get_P(Public_Key_A.E) 
				      - Big_Unsigned_Three) + Big_Unsigned_One;
      Private_Key_A.Q := Private_Key_A.d * Public_Key_A.P;
      Public_Key_A.Q := Private_Key_A.Q ;
   end Gen_Single_Private_Key;

-------------------------------------------------------------------------------

   procedure Gen_Shared_Private_Key(Public_Key_B  : in Public_Key_ECDH;
				    Private_Key_A : in Private_Key_ECDH;
				    Shared_Key_A  : out Shared_Key_ECDH) is
      tmp_EC 			: EC_Point;
      EC_IFINITY_EX	: exception;
   begin
      tmp_EC := Private_Key_A.d * Public_Key_B.Q;
      if tmp_EC = EC_Point_Infinity then
	 raise EC_IFINITY_EX with "Shared Key is Infinity";
      else
	 Shared_Key_A.W := tmp_EC;
      end if;
   end Gen_Shared_Private_Key;
   
-------------------------------------------------------------------------------

   function Verify(Public_Key_A  : Public_Key_ECDH;
		   Public_Key_B  : Public_Key_ECDH;
		   Private_Key_A : Private_Key_ECDH;
		   Private_Key_B : Private_Key_ECDH;
		   Shared_Key_A  : Shared_Key_ECDH;
		   Shared_Key_B  : Shared_Key_ECDH) return Boolean is
      tmp_bool_1, tmp_bool_2, Tmp_Bool_3 : Boolean := false;
      tmp_bool_4, tmp_bool_5 : Boolean := false;
   begin
      if equal_Public_Key_Curve(Public_Key_A, Public_Key_B) then
	 tmp_bool_1 := true;
      end if;
      if Private_Key_A.Q = Private_Key_A.d * Public_Key_A.P and 
	Public_Key_A.Q = Private_Key_A.Q and
	Private_Key_B.Q = Private_Key_B.d * Public_Key_B.P and 
	Public_Key_B.Q = Private_Key_B.Q then
	 tmp_bool_2 := true;
      end if;
      if Shared_Key_A.W = Private_Key_A.d * Public_Key_B.Q then
	 tmp_bool_3 := true;
      end if;
      if Shared_Key_B.W = Private_Key_B.d * Public_Key_A.Q then
	 tmp_bool_4 := true;
      end if;
      if Shared_Key_A.W = Shared_Key_B.W then
	 tmp_bool_5 := true;
      end if;
      if tmp_bool_1 and tmp_bool_2 and tmp_bool_3 and tmp_bool_4 and 
	tmp_bool_5 then
	 return true;
      else
	 return false;
      end if;
   end Verify;

-------------------------------------------------------------------------------

	function Equal_Public_Key_Curve(Public_Key_A  : Public_Key_ECDH;
					Public_Key_B  : Public_Key_ECDH) 
	  return Boolean is
	begin
	   if (Public_Key_A.E = Public_Key_B.E) and 
	     (Public_Key_A.P = Public_Key_B.P) and 
	     (Public_Key_A.n = Public_Key_B.n) then
	      return true;
	   else
	      return false;
	   end if;
	end equal_Public_Key_Curve;
-------------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.ECDH;
