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

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Crypto.Asymmetric.ECDH;
with Crypto.Types;

use Crypto.Types;

generic
   Size : Positive;

package Crypto.Asymmetric.ECIES is

	package ECDH is new Crypto.Asymmetric.ECDH(Size);
	use ECDH;
	use Big;
	use Big.Utils;
	use EC;
	use Zp;
	use DB;

   type Cipher_ECIES is private;
   type Plain_ECIES is private;

-------------------------------------------------------------------------------

   --internal purpose
   procedure Message_Prepare(Plain   : out Plain_ECIES;
                             Message : in  String);

   --internal purpose
   procedure Key_Prepare(AES_Key    : out B_Block256;
                         Mac_Key    : out W_Block512;
                         Shared_Key : in  Shared_Key_ECDH);


   --internal purpose
   procedure Mac_Compute(Mac_Key : in  W_Block512;
                         Cipher  : in  Cipher_ECIES;
                         Mac     : out W_Block256);

   procedure Encrypt(Public_Key_A : in  Public_Key_ECDH;
                     Shared_Key   : in  Shared_Key_ECDH;
                     Plaintext	 : in  String;
                     Cipher       : out Cipher_ECIES);

   procedure Decrypt(Shared_Key	: in  Shared_Key_ECDH;
                     Cipher      : in  Cipher_ECIES;
                     Plaintext   : out Unbounded_String);


   procedure Encrypt(Public_Key_A  : in  Public_Key_ECDH;
                     Public_Key_B  : in  Public_Key_ECDH;
                     Private_Key_A : in  Private_Key_ECDH;
                     Plaintext	  : in  String;
                     Cipher        : out Cipher_ECIES);

   procedure Decrypt(Public_Key_B  : in  Public_Key_ECDH;
                     Private_Key_A : in  Private_Key_ECDH;
                     Cipher        : in  Cipher_ECIES;
                     Plaintext     : out Unbounded_String);

-------------------------------------------------------------------------------

private

	package Container_Message_Map is new Ada.Containers.Ordered_Maps
		(Key_Type => natural,
		 Element_Type => B_Block128);

	package Container_Cipher_Map is new Ada.Containers.Ordered_Maps
		(Key_Type => natural,
		 Element_Type => B_Block128);


   type ECIES_C_KEY is record
      Public_Point        : EC_Point;
      Message_Block_Count : natural := 0;
      Mac                 : W_Block256;
      Cipher              : Unbounded_String;
      Cipher_Map          : Container_Cipher_Map.Map;
   end record;
   type Cipher_ECIES is new ECIES_C_KEY;

   type ECIES_P_KEY is record
      Message_Block_Count : natural := 1;
      AES_Key             : B_Block256;
      Mac_Key             : W_Block512;
      Message             : Unbounded_String;
      Message_Map         : Container_Message_Map.Map;
   end record;
   type Plain_ECIES is new ECIES_P_KEY;

   pragma Optimize (Time);

end Crypto.Asymmetric.ECIES;
