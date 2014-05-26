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
with Crypto.Symmetric.Hashfunction_SHA1;
with Crypto.Symmetric.Mac.Hmac_SHA1;
with Crypto.Debug_Put;

pragma Elaborate_All (Crypto.Symmetric.KDF);
with Crypto.Symmetric.Mac.Hmac;


generic
   with package Hmac_Package is new Crypto.Symmetric.Mac.Hmac(<>);
   with function To_Message_Type(B : Bytes) return Hmac_Package.H.Message_Type;
   with function To_Bytes(M : Hmac_Package.H.Hash_Type) return Bytes;
   with function "xor"(Left : Hmac_Package.H.Hash_Type;
                       Right: Hmac_Package.H.Hash_Type)
                       return Hmac_Package.H.Hash_Type;
package Crypto.Symmetric.KDF_PBKDF2.Testing is

   type PBKDF2_KDF_Testing is new PBKDF2_KDF with private;

private

   package Error_Output is new Crypto.Debug_Put(b => false);

   --actual derivation function, pure PBKDF2
   procedure PBKDF2_Testing(This	: in out PBKDF2_KDF_Testing;
                            Salt	: in 	Bytes;
                            Password	: in	Bytes;
                            Key		: out	Bytes;
                            DK_Len	: in 	Natural);

   type PBKDF2_KDF_Testing is new PBKDF2_KDF with null record;
end Crypto.Symmetric.KDF_PBKDF2.Testing;
