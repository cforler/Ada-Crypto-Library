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

generic

package Crypto.Symmetric.AE.AD is

   type AEAD_Scheme is limited interface;

   procedure Encrypt(This             : in out AEAD_Scheme;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is abstract;

   -- Keep in mind that if you're using Decrypt_And_Verify with Read_Ciphertext_Again
   -- as default, the whole Ciphertext will be stored in main memory.

   -- Keep in mind that if you're using Decrypt_And_Verify without specifying a
   -- Callback_Reader for Read_Ciphertext_Again, the whole Ciphertext will be stored
   -- in Main Memory.
   function Decrypt_And_Verify(This                   : in out AEAD_Scheme;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader
                               	:= null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is abstract;

end Crypto.Symmetric.AE.AD;
