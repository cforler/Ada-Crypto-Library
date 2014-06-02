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

with Crypto;
with Crypto.Types;
with Crypto.Types.Big_Numbers;

use Crypto.Types;

generic
   Size : Positive;

package Crypto.Asymmetric.RSA is

   package Big is new Crypto.Types.Big_Numbers(Size);
   use Big;

   ---------------------------------------------------------------------------
   ---------------------------TYPES-------------------------------------------
   ---------------------------------------------------------------------------

   subtype RSA_Number is Bytes(0..Size/8-1);
   type Public_Key_RSA is private;
   type Private_Key_RSA is private;

   ---------------------------------------------------------------------------
   --------------------------HIGH LEVEL API-----------------------------------
   ---------------------------------------------------------------------------

   procedure Gen_Key(Public_Key  : out Public_Key_RSA;
                     Private_Key : out Private_Key_RSA;
		     Small_Default_Exponent_E : in Boolean := True);

   ---------------------------------------------------------------------------

   function Verify_Key_Pair(Private_Key : Private_Key_RSA;
                            Public_Key  : Public_Key_RSA) return Boolean;

   ---------------------------------------------------------------------------


   function OAEP_Encrypt(Public_Key : in  Public_Key_RSA;
                         Plaintext  : in  Bytes) return RSA_Number;


   function OAEP_Decrypt(Private_Key : in  Private_Key_RSA;
                         Ciphertext  : in  RSA_Number) return Bytes;



   ---------------------------------------------------------------------------

   -- N = (P-1) * (Q-1); N = PQ;  ED = 1 (mod Phi)

   procedure Get_Public_Key(Public_Key : in Public_Key_RSA;
                            N : out RSA_Number;
                            E : out RSA_Number);
   
   procedure Get_Private_Key(Private_Key : in Private_Key_RSA;
                             N   : out RSA_Number;
                             D   : out RSA_Number;
                             P   : out RSA_Number;
                             Q   : out RSA_Number;
                             Phi : out RSA_Number);


   ---------------------------------------------------------------------------

   procedure Set_Public_Key(N : in RSA_Number;
                            E : in RSA_Number;
                            Public_Key : out Public_Key_RSA);
   
   procedure Set_Public_Key(N : in Big_Unsigned;
                            E : in Big_Unsigned;
                            Public_Key : out Public_Key_RSA);
      

   procedure Set_Private_Key(N   : in RSA_Number;
                             D   : in RSA_Number;
			     P   : in RSA_Number;
			     Q   : in RSA_Number;
                             Phi : in RSA_Number;
                             Private_Key : out Private_Key_RSA);
         
   procedure Set_Private_Key(N   : in Big_Unsigned;
                             D   : in Big_Unsigned;
			     P   : in Big_Unsigned;
			     Q   : in Big_Unsigned; 
                             Phi : in Big_Unsigned;
                             Private_Key : out Private_Key_RSA);

   ---------------------------------------------------------------------------
   ------------------------------LOW LEVEL API--------------------------------
   ---------------------------------------------------------------------------

   procedure Encrypt(Public_Key : in  Public_Key_RSA;
                     Plaintext  : in  RSA_Number;
                     Ciphertext : out RSA_Number);
   
   procedure Encrypt(Public_Key  : in  Public_Key_RSA;
                     Plaintext   : in  Big_Unsigned;
                     Ciphertext  : out Big_Unsigned);

   procedure Decrypt(Private_Key : in  Private_Key_RSA;
                      Ciphertext  : in  RSA_Number;
                      Plaintext   : out RSA_Number);

   procedure Decrypt(Private_Key : in  Private_Key_RSA;
                     Ciphertext  : in  Big_Unsigned;
                     Plaintext   : out Big_Unsigned);

   ---------------------------------------------------------------------------
   ------------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------
   
private
   
   type Public_Key_RSA is record
      N : Big_Unsigned;
      E : Big_Unsigned;
   end record;
   
   type Private_Key_RSA is record
      N : Big_Unsigned;
      D : Big_Unsigned;
      P : Big_Unsigned;
      Q : Big_Unsigned; 
      Phi : Big_Unsigned; --= p-1*q-1
   end record;
   
   pragma Optimize (Time);

end Crypto.Asymmetric.RSA;
