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

with Ada.Text_IO;

package body Crypto.Symmetric.KDF_PBKDF2.Testing is


   --actual derivation function, pure PBKDF2
   procedure PBKDF2_Testing(This	: in out PBKDF2_KDF_Testing;
                            Salt	: in 	Bytes;
                            Password	: in	Bytes;
                            Key		: out	Bytes;
                            DK_Len	: in 	Natural) is
   begin
      This.PBKDF2(Salt     => Salt,
                  Password => Password,
                  Key      => Key,
                  DK_Len   => DK_Len);
   end PBKDF2_Testing;




end Crypto.Symmetric.KDF_PBKDF2.Testing;
