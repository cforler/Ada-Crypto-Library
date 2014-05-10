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

package body Crypto.Debug_Put is


   procedure Put_Line(S : in String) is
   begin
      if b then
         Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                              Item => S);
      end if;
   end;


   procedure Put(S : in String) is
   begin
      if b then
         Ada.Text_IO.Put(File => Ada.Text_IO.Standard_Error,
                              Item => S);
      end if;
   end;

   procedure Put(Item  : Integer;
                 Width : Field := Default_Width;
                 Base  : Number_Base := Default_Base) is
   begin
      if b then
         Ada.Integer_Text_IO.Put(Item  => Item,
                                 Width => Width,
                                 Base  => Base);
      end if;
   end Put;

   procedure New_Line is
   begin
      if b then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;



end Crypto.Debug_Put;
