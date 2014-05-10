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
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Debug_Put;
pragma Elaborate_All (Crypto.Symmetric.KDF);


package Crypto.Symmetric.KDF_Scrypt is

   type W_Block512_Array is array (Integer range <>) of W_Block512;

   package Error_Output is new Crypto.Debug_Put(b => false);

   array_size_not_equal_exception : exception ;
   N_not_power_of_2_exception : exception;

   package KDF is new Crypto.Symmetric.KDF(Return_Type        => W_Block512,
                                           H                  => Crypto.Symmetric.Hashfunction_SHA512);
   use KDF;

   type Scrypt_KDF is new KDF.KDF_Scheme with private;


   --Interface function for static 64 Bytes Output, assuming p=8, r=8 and N=Security_Parameter
   overriding
   procedure Derive(This	: in out Scrypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	W_Block512);

   --function for setting security parameter, used here for setting round count
   overriding
   procedure Initialize(This	: out Scrypt_KDF;
                       Key_Length: in Natural);


   procedure Initialize (This	: out Scrypt_KDF;
                         r		: in 	Natural;
                         N		: in 	Natural;
                         p		: in	Natural;
                         dkLen	: in	Natural);

   --core scrypt function
   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes);

   --Block rearrangement, used by Scrypt_Block_Mix
   function Scrypt_ROMix(Input	: in 	W_Block512_Array;
                         N	: in 	Natural) return W_Block512_Array;

   --Function Scrypt_Block_Mix, used by scrypt
   function Scrypt_Block_Mix(Input	: in W_Block512_Array) return W_Block512_Array;

   --Stream Cipher, used by Scrypt_Block_Mix
   procedure Salsa20_8(Input	: in	W_Block512;
                       Output 	: out	W_Block512);

   --XORing function for type W_Block512_Array
   function "xor" (Left : W_Block512_Array;
                   Right: W_Block512_Array) return W_Block512_Array;

   --power of two test (rudimentary)
   function IsPowerOfTwo(value : Natural) return Boolean;

   private
   type Scrypt_KDF is new KDF.KDF_Scheme with
      record
         r 			: Natural :=8;
         N 			: Natural :=8;
         p 			: Natural :=8;
         dkLen 			: Natural;

      end record;


end Crypto.Symmetric.KDF_Scrypt;
