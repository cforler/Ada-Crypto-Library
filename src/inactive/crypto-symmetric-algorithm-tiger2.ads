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

-- This Tiger implementation is based on the research paper
--"Tiger: A Fast New Hash function"  from Ross Anderson1 and Eli Biham2

package Crypto.Hashfunction.Tiger2 is


   -- low level API
   procedure Init_Tiger2(Hash_Value : out DW_Block192);

   procedure Round_Tiger2(Message_Block : in DW_Block512;
                         Hash_Value    : in out DW_Block192);

   function Final_Round_Tiger2(Last_Message_Block  : DW_Block512;
                               Last_Message_Length : Message_Block_Length512;
                               Hash_Value          : DW_Block192)
                              return DW_Block192;

   -- high level API
   procedure Tiger2(Message : in Bytes;  Hash_Value :  out DW_Block192);

   procedure Tiger2(Message : in String; Hash_Value :  out DW_Block192);

  --   procedure F_Tiger2(Filename : in String; Hash_Value :  out DW_Block192);


   Tiger2_Constraint_Error : exception;

end Crypto.Hashfunction.Tiger2;
