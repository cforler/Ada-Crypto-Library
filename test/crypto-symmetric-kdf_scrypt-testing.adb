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

with Crypto.Types;
use Crypto.Types;



package body Crypto.Symmetric.KDF_Scrypt.Testing is

   procedure scrypt_Testing (Password 	: in 	String;
                             Salt 	: in 	String;
                             r		: in 	Natural;
                             N		: in 	Natural;
                             p		: in	Natural;
                             dkLen	: in	Natural;
                             Key	: out 	Bytes) is
   begin
      scrypt(Password => Password,
             Salt     => Salt,
             r        => r,
             N        => N,
             p        => p,
             dkLen    => dkLen,
             Key      => Key);
   end scrypt_Testing;

   --Block rearrangement, used by Scrypt_Block_Mix
   function Scrypt_ROMix_Testing(Input	: in 	W_Block512_Array;
                                 N	: in 	Natural) return W_Block512_Array is
   begin
      return Scrypt_ROMix(Input => Input,
                          N     => N);
   end Scrypt_ROMix_Testing;


   --Function Scrypt_Block_Mix, used by scrypt
   function Scrypt_Block_Mix_Testing(Input	: in W_Block512_Array)
                                     return W_Block512_Array is
   begin
      return Scrypt_Block_Mix(Input => Input);
   end Scrypt_Block_Mix_Testing;


   --Stream Cipher, used by Scrypt_Block_Mix
   procedure Salsa20_8_Testing(Input	: in	W_Block512;
                               Output 	: out	W_Block512) is
   begin
      Salsa20_8(Input  => Input,
                Output => Output);
   end Salsa20_8_Testing;

   --XORing function for type W_Block512_Array
   function xor_Testing (Left : W_Block512_Array;
                         Right: W_Block512_Array) return W_Block512_Array is
   begin

      return Left xor Right;
   end xor_Testing;




   --power of two test (rudimentary)
   function IsPowerOfTwo_Testing(value : Natural) return Boolean is
   begin
      return IsPowerOfTwo(value => value);
   end IsPowerOfTwo_Testing;




end Crypto.Symmetric.KDF_Scrypt.Testing;
