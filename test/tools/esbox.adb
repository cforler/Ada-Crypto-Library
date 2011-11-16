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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with  Crypto.Symmetric.Algorithm.TripleDES.Tables;
use   Crypto.Symmetric.Algorithm.TripleDES.Tables;
with  Crypto.Symmetric.Algorithm.TripleDES;
use   Crypto.Symmetric.Algorithm.TripleDES;
with Crypto;  use Crypto;
with Interfaces; use Interfaces;


procedure Esbox is
   A : Bit6:=2#011011#;
   type Naturals is array(0..63) of Natural;
   N : Naturals;
   Row, Column : Byte;

   package B6 is new Ada.Text_Io.Modular_IO (Bit6);
   package Bio is new Ada.Text_Io.Modular_IO (Byte);

begin
   -- S(row, column)


   for I in 0..63 loop
      Row := Shift_Left((Shift_Right(Byte((I)),5) mod 2),1)
        xor (Byte(I) mod 2);
      Column := Shift_Right(Byte(I),1) and 2#1111#;

      N(I):=S8(Natural(Row), Natural(Column));

--      Put("I: "); Put(I,   Base => 2, Width => 3);
--      New_Line;
--      Bio.Put(Row, Width => 3 );  Bio.Put(Column);
--      New_Line;
   end loop;



   New_Line;
   -- output: ES-Box
   for I in  0..3 loop
      for j in 0..15 loop
         Put(N(J+I*16), Width => 3);
         Put(",");
      end loop;
      New_Line;
   end loop;
   New_Line;


end Esbox;
