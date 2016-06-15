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

-- All the procedures of this package based on FIPS 180-2

with Ada.Text_IO; use Ada.Text_IO;

package body Crypto.Types.Output is 

procedure Put_B_Block_128(input : B_Block128) is
tmpBytes : Bytes(0..15);
begin
	tmpBytes := To_Bytes(input);
      for i in tmpBytes'range loop
         Put(To_Hex(tmpBytes(i)));
      end loop;
      New_Line;

end Put_B_Block_128;

procedure Put_DW_Block8192(input : DW_Block8192) is
tmpBytes : Bytes(0..1023);
begin
	tmpBytes := To_Bytes(input);
      for i in tmpBytes'range loop
         Put(To_Hex(tmpBytes(i)));
      end loop;
      New_Line;

end Put_DW_Block8192;

procedure Put_DW_Block1024(input : DW_Block1024) is
tmpBytes : Bytes(0..127);
begin
   tmpBytes := To_Bytes(input);
      for i in tmpBytes'range loop
         Put(To_Hex(tmpBytes(i)));
      end loop;
      New_Line;

end Put_DW_Block1024;

procedure Put_Bytes(input : Bytes) is
begin
   for i in natural range Input'range loop
      Put(To_Hex(input(i)));
   end loop;
      New_Line;
end;


end Crypto.Types.Output;