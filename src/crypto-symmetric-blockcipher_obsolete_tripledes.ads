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


with Crypto.Symmetric.Algorithm.Tripledes;
with Crypto.Symmetric.Blockcipher;
with Crypto.Types;

use Crypto.Types;
use Crypto.Symmetric.Algorithm.Tripledes;

pragma Elaborate_All (Crypto.Symmetric.Blockcipher);

package Crypto.Symmetric.Blockcipher_Obsolete_TripleDES is
   new  Crypto.Symmetric.Blockcipher(Block          => B_Block64,
                                     Key_Type       => B_Block192,
                                     Cipherkey_Type => Obsolete_Cipherkey_TDES,
                                     Prepare_Key    => Obsolete_Prepare_Key,
                                     Encrypt => Obsolete_Encrypt,
                                     Decrypt => Obsolete_Decrypt,
				     To_Key_Type   => To_B_Block192,
				     To_Block_Type => To_B_Block64,
				     Block_To_Bytes => To_Bytes);
