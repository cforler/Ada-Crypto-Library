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


package Crypto.Symmetric.KDF_Catena.Testing is

   package Error_Output is new Crypto.Debug_Put(b => false);

  function bitreverse_testing(x  : in Integer;
                      n  : in Integer) return Integer;

  -- procedure Flap_testing( x     : in Bytes;
  --                 g     : in Integer;
  --                 salt  : in Bytes;
  --                 hash  : out Digest_Type;
  --                 structure: in String;
  --                 mode      :in String;
  --                 phi       : in String);

  -- function HashFast_testing (Left : Digest_Type;
  --                    Right: Digest_Type) return Digest_Type;

  -- procedure InitMem_testing(  x     : in Bytes;
  --                     g     : in Integer;
  --                     r     : in out State_Array);

  -- procedure gamma_testing(  g   : in Natural;
  --                   salt: in Bytes;
  --                   r   : in out State_Array_Access);


end Crypto.Symmetric.KDF_Catena.Testing;
