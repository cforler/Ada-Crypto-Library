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

with Crypto.Types.Random_Source;
use Crypto.Types;

package Crypto.Types.Random is
   procedure Set(Source : in Crypto.Types.Random_Source.Random_Source'Class);
   
   procedure Read(B : out Byte);
   procedure Read(Byte_Array : out Bytes);
   procedure Read(B : out B_Block128);

   procedure Read(W : out Word);
   procedure Read(Word_Array : out Words);

   procedure Read(D : out DWord);
   procedure Read(DWord_Array : out DWords);

   pragma Inline (Read);
   pragma Optimize (Time);
end Crypto.Types.Random;
