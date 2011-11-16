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

with Ada.Containers.Ordered_Maps;

generic
package Crypto.Types.Elliptic_Curves.Zp is
   use Big;
   
   package Elliptic_Curve_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Big_Unsigned,
      Element_Type => EC_Point);
   
   
   type Elliptic_Curve_Zp is private;
   
   -- init an elliptic curve over Z_p
   procedure Init(A, B, P : in Big_Unsigned);

   procedure Init(ECZ : in Elliptic_Curve_Zp);

   procedure Init(Len : Positive);

   function Is_Elliptic_Curve return Boolean;

   function On_Elliptic_Curve(X : EC_Point) return Boolean;

   function Negative(X : EC_Point) return EC_Point;

   function Is_Negative(X : EC_Point) return Boolean;

   function Is_Negative(Left, Right : EC_Point) return Boolean;

   function "+"(Left, Right : EC_Point) return EC_Point;

   function "-"(Left, Right : EC_Point) return EC_Point;

   function Double(X : EC_Point) return EC_Point;

   function "*"(Left : Big_Unsigned; Right : EC_Point) return EC_Point;
   
   function Get_P(ECZ : in Elliptic_Curve_Zp) return  Big_Unsigned;
   
   ---------------------------------------------------------------------------
   --------------------------------PRIVATE------------------------------------
   ---------------------------------------------------------------------------
 
private
      type Elliptic_Curve_Zp is record
	 A : Big_Unsigned;
	 B : Big_Unsigned;
	 P : Big_Unsigned;
      end record;
      
      pragma Inline (Get_P);

end Crypto.Types.Elliptic_Curves.Zp;
