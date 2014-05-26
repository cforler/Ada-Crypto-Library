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



package body Crypto.Symmetric.KDF_SHA512Crypt.Testing is

   --Adding Bytes to Digest
   procedure Add_Bytes_Testing
     (Bytes_To_Add		: in 		Bytes;
      Digest_Bytes		: in out 	Bytes;
      Digest_Bytes_Length	: in out	Natural;
      Digest_Hash   : in out Crypto.Symmetric.Algorithm.SHA512.Sha512_Context)is
   begin
      Add_Bytes(Bytes_To_Add        => Bytes_To_Add,
                Digest_Bytes        => Digest_Bytes,
                Digest_Bytes_Length => Digest_Bytes_Length,
                Digest_Hash         => Digest_Hash);
   end Add_Bytes_Testing;



end Crypto.Symmetric.KDF_SHA512Crypt.Testing;
