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

with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.Zp;
with Crypto.Types.Elliptic_Curves.Zp.Database;

use Crypto.Types;

generic
   Size : Positive;
   Rand_Source : String := "";


package Crypto.Asymmetric.ECDH is

   package Big is new Crypto.Types.Big_Numbers(Size, Rand_Source);
   use Big;
   package EC  is new Crypto.Types.Elliptic_Curves(Big);
   use EC;
   package Zp is new EC.Zp;
   use Zp;
   package DB is new Zp.Database;
   use DB;


   type Private_Key_ECDH is private;

   type ECDH_KEY is record
      W : EC_Point; --x,y
   end record;

   type Shared_Key_ECDH is new ECDH_KEY;

   type ECDH_P_KEY is record
      E : Elliptic_Curve_Zp;
      P : EC_Point; --x,y
      n : Big_Unsigned;
      Q : EC_Point; --x,y
   end record;

   type Public_Key_ECDH is new ECDH_P_KEY;

-------------------------------------------------------------------------------

   procedure Gen_Public_Key(Public_Key_A  : out Public_Key_ECDH;
			    length        : in DB.Bit_Length);
   
   procedure Gen_Single_Private_Key(Public_Key_A  : in out Public_Key_ECDH;
				    Private_Key_A : out  Private_Key_ECDH);
   
   procedure Gen_Shared_Private_Key(Public_Key_B  : in Public_Key_ECDH;
				    Private_Key_A : in Private_Key_ECDH;
				    Shared_Key_A : out Shared_Key_ECDH);
   
   function Verify(Public_Key_A  : Public_Key_ECDH;
		   Public_Key_B  : Public_Key_ECDH;
		   Private_Key_A : Private_Key_ECDH;
		   Private_Key_B : Private_Key_ECDH;
		   Shared_Key_A : Shared_Key_ECDH;
		   Shared_Key_B : Shared_Key_ECDH) return Boolean;
   
   function equal_Public_Key_Curve(Public_Key_A  : Public_Key_ECDH;
				   Public_Key_B  : Public_Key_ECDH) 
				  return Boolean;
   
   function Is_Init(Key : ECDH_P_KEY) return Boolean;
   
  ----------------------------------------------------------------------------
private

   type ECDH_S_KEY is record
      Q : EC_Point; --x,y
      d : Big_Unsigned;
   end record;

   type Private_Key_ECDH is new ECDH_S_KEY;
   
   pragma Inline (Gen_Public_Key,Gen_Single_Private_Key,Equal_Public_Key_Curve);
   pragma Optimize (Time);


end Crypto.Asymmetric.ECDH;
