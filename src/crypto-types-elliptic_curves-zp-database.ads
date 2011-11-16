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
package Crypto.Types.Elliptic_Curves.Zp.Database is
   type Bit_Length is new natural;
   
    -- curves of the type E: y*y = x*x*x - 3*x + b (mod p)
   type Precomputed_Elliptic_Curve is record
      P : String(1..192) := (others=>' ');	-- prime modulus
      R : String(1..192) := (others=>' '); 	-- order
      S : String(1..192) := (others=>' '); 	-- 160-bit input seed to SHA-1
      C : String(1..192) := (others=>' '); 	-- output of SHA-1
      B : String(1..192) := (others=>' '); 	-- coefficient b (satisfying b*b*c = -27 (mod p))
      Gx : String(1..192) := (others=>' '); 	-- base point x coordinate
      Gy : String(1..192) := (others=>' '); 	-- base point x coordinate
      length : Bit_Length;								-- Bit lenght
   end record;
   
   package Elliptic_Curve_Database_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Bit_Length,
      Element_Type => Precomputed_Elliptic_Curve);


   procedure Get_Elliptic_Curve(ECZ    : out Elliptic_Curve_Zp;
                                ECP    : out EC_Point;
                                order  : out Big_Unsigned;
                                length : in  Bit_Length);

   --for internal purpose
   procedure Set_Elliptic_Curve_Map;

   ECD : Elliptic_Curve_Database_Map.Map;
   
end Crypto.Types.Elliptic_Curves.Zp.Database;
