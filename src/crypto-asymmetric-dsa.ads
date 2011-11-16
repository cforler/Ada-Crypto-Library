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

-- This implementation is based on the FIPS FUB 186-2.
-- Exception: Generation of primes:
-- I use a simpler way to generate the two primes P, and Q with P = 1 mod 2Q.

with Crypto.Types.Big_Numbers;

use Crypto.Types;

generic
   Size : Positive;

package Crypto.Asymmetric.DSA is

   package Big is new Crypto.Types.Big_Numbers(Size);
   use Big;

   ---------------------------------------------------------------------------
   ---------------------------TYPES-------------------------------------------
   ---------------------------------------------------------------------------

   subtype DSA_Number is Bytes(0..Size/8-1);
   type Public_Key_DSA is private;
   type Private_Key_DSA is private;
   type Signature_DSA is private;

   ---------------------------------------------------------------------------
   -------------------------METHODS-------------------------------------------
   ---------------------------------------------------------------------------

   procedure Gen_Key(Public_Key  : out Public_Key_DSA;
                     Private_Key : out Private_Key_DSA);

   procedure Sign(Private_Key : in  Private_Key_DSA;
                  SHA1_Hash   : in  W_Block160;
                  Signature   : out Signature_DSA);

   function Verify(Public_Key  : Public_Key_DSA;
                   SHA1_Hash   : W_Block160;
                   Signature   : Signature_DSA) return Boolean;

   ---------------------------------------------------------------------------

   procedure Sign_File(Filename    : in  String;
                       Private_Key : in  Private_Key_DSA;
                       Signature   : out Signature_DSA);

   function Verify_File(Filename   : String;
                        Public_Key : Public_Key_DSA;
                        Signature  : Signature_DSA) return Boolean;

   function Verify_Key_Pair(Private_Key : Private_Key_DSA;
                            Public_Key  : Public_Key_DSA) return Boolean;


   ---------------------------------------------------------------------------

   -- If you do not understand the following comments then read the fips 186-2.
   -- P = a prime modulus, where 2**(Size-1) < P < 2**Size
   -- Q = a prime modulus, where 2**159 < Q < 2*160 and p =1 (mod 2Q)
   -- G = H**((p-1)/q) mod p
   -- 0 < Y < Q
   -- Y = G**X mod P


   procedure Get_Public_Key(Public_Key : in Public_Key_DSA;
                            P : out DSA_Number;
                            Q : out DSA_Number;
                            G : out DSA_Number;
                            Y : out DSA_Number);

   procedure Get_Private_Key(Private_Key : in Private_Key_DSA;
                             P : out DSA_Number;
                             Q : out DSA_Number;
                             G : out DSA_Number;
                             X : out DSA_Number);

   procedure Get_Signature(Signature : in Signature_DSA; 
                           R         : out Big_Unsigned; 
                           S         : out Big_Unsigned);

   procedure Set_Public_Key(P : in DSA_Number;
                            Q : in DSA_Number;
                            G : in DSA_Number;
                            Y : in DSA_Number;
                            Public_Key : out Public_Key_DSA);

   procedure Set_Private_Key(P : in DSA_Number;
                             Q : in DSA_Number;
                             G : in DSA_Number;
                             X : in DSA_Number;
                             Private_Key : out Private_Key_DSA);

   procedure Set_Signature(R         : in Big_Unsigned; 
                           S         : in Big_Unsigned;
                           Signature : out Signature_DSA);

   ---------------------------------------------------------------------------
   ------------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------

private

   type DSA_KEY is record
      P : Big_Unsigned;
      Q : Big_Unsigned;
      G : Big_Unsigned;
      K : Big_Unsigned; --x,y
   end record;

   type Public_Key_DSA is new DSA_KEY;
   type Private_Key_DSA is new DSA_KEY;

   type Signature_DSA is record
      R : Big_Unsigned;
      S : Big_Unsigned;
   end record;

   pragma Optimize (Time);

end  Crypto.Asymmetric.DSA;

