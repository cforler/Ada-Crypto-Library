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
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction;

use Crypto.Types;
use Crypto.Symmetric.Algorithm.SHA512;


pragma Elaborate_All (Crypto.Symmetric.Hashfunction);
package Crypto.Symmetric.Hashfunction_SHA512 is
  new Crypto.Symmetric.Hashfunction(Hash_Type                 => DW_Block512,
                                           Message_Type              => DW_Block1024,
                                           Message_Block_Length_Type => Crypto.Types.Message_Block_Length1024,
                                           Internal_Scheme           => Sha512_Interface,
                                           Generic_To_Bytes	     => To_Bytes);
