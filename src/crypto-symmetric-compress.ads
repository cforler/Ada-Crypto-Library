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

package Crypto.Symmetric.Compress is
   type Compress_Scheme is abstract tagged
   record
   	Length : Natural;
   end record;

  function Process(This     : in out Compress_Scheme;
                     Input_1   : in Bytes;
                     Input_2   : in Bytes;
                     vindex    : in Natural) return Bytes is abstract;

  function Get_Length(This     : in out Compress_Scheme) return Integer is abstract;

  procedure Reset(This     : in out Compress_Scheme) is abstract;

  end Crypto.Symmetric.Compress;