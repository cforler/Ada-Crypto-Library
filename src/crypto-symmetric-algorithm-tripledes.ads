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


package Crypto.Symmetric.Algorithm.Tripledes is

   -- This TDES implementation is based on the sourcecode from
   -- Bruce Schneiers "Applied Cryptography"

   type Cipherkey_TDES is private;

   procedure Prepare_Key(Key : in B_Block192;
                         Cipherkey : out Cipherkey_TDES);


   procedure Encrypt(Cipherkey  : in  Cipherkey_TDES;
                     Plaintext  : in  B_Block64;
                     Ciphertext : out B_Block64);

   procedure Decrypt(Cipherkey  : in  Cipherkey_TDES;
                     Ciphertext : in  B_Block64;
                     Plaintext  : out B_Block64);



   ---------------------------------------------------------------------------
   -------------------------------PRIVATE-------------------------------------
   ---------------------------------------------------------------------------

private

   subtype Roundkey_DES is Words(0..31);

   type Cipherkey_DES is
      record
         Encrypt_Key : Roundkey_DES;
         Decrypt_Key : Roundkey_DES;
      end record;

   type Cipherkey_TDES is  array(0..2) of Cipherkey_DES;



   procedure Build_Roundkey(Key : in B_Block64; Kn : out Roundkey_DES;
						Direction : in Direction_type);
   
   procedure Build_Encrypt_Key(Key : in B_Block64; Kn : out Roundkey_DES);
   procedure Build_Decrypt_Key(Key : in B_Block64; Kn : out Roundkey_DES);
   
   function Des(Roundkey : in Roundkey_DES; Input : in B_Block64)
	       return B_Block64;
   
   pragma Inline (Prepare_Key, Encrypt, Decrypt);
   pragma Optimize (Time);   
end Crypto.Symmetric.Algorithm.TripleDES;

