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

with Crypto.Types; use Crypto.Types;
with Crypto.Symmetric.KDF;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Debug_Put;
with Crypto.Types.Base64;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_SHA512Crypt is

   type Base64_Character is
     ('.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8' ,'9', 'A', 'B',
         'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
         'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
         'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
     's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '=');

   package Base64 is new Crypto.Types.Base64
     (Base64_Character => Base64_Character);

   package KDF is new Crypto.Symmetric.KDF
     (Return_Type        => Base64.Base64_SHA512Crypt);

   type SHA512Crypt_KDF is new KDF.KDF_Scheme with private;

   --Interface and core function
   overriding
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Base64.Base64_SHA512Crypt);


   --function for setting Key Length
   overriding
   procedure Initialize(This	: out SHA512Crypt_KDF;
                        Key_Length: in Natural);

   --function for setting Key Length and security parameter,
   -- used here for setting round count in F_Function
   procedure Initialize(This		: out SHA512Crypt_KDF;
                        Key_Length	: in Natural;
                        Round_Count	: in Natural);


private

   --Adding Bytes to Digest
   procedure Add_Bytes
     (Bytes_To_Add		: in 		Bytes;
      Digest_Bytes		: in out 	Bytes;
      Digest_Bytes_Length	: in out	Natural;
      Digest_Hash    : in out Crypto.Symmetric.Algorithm.SHA512.Sha512_Context);

   package Error_Output is new Crypto.Debug_Put(b => false);
   type SHA512Crypt_KDF is new KDF.KDF_Scheme with
      record
         Key_Length	: Natural;
         Round_Count	: Natural :=5000;
      end record;

   function Natural_To_Binary_String(N: Natural) return String;

end Crypto.Symmetric.KDF_SHA512Crypt;
