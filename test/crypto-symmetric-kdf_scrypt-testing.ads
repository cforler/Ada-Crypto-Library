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

with Crypto.Debug_Put;


package Crypto.Symmetric.KDF_Scrypt.Testing is

   package Error_Output is new Crypto.Debug_Put(b => false);

   array_size_not_equal_exception : exception ;
   N_not_power_of_2_exception : exception;

   procedure scrypt_Testing (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes);

   --Block rearrangement, used by Scrypt_Block_Mix
   function Scrypt_ROMix_Testing(Input	: in 	W_Block512_Array;
                         N	: in 	Natural) return W_Block512_Array;

   --Function Scrypt_Block_Mix, used by scrypt
   function Scrypt_Block_Mix_Testing(Input	: in W_Block512_Array)
                             return W_Block512_Array;

   --Stream Cipher, used by Scrypt_Block_Mix
   procedure Salsa20_8_Testing(Input	: in	W_Block512;
                       Output 	: out	W_Block512);

   --power of two test (rudimentary)
   function IsPowerOfTwo_Testing(value : Natural) return Boolean;

   --XORing function for type W_Block512_Array
   function xor_Testing (Left : W_Block512_Array;
                         Right: W_Block512_Array) return W_Block512_Array;


end Crypto.Symmetric.KDF_Scrypt.Testing;
