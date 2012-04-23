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

generic
   type Hash_Type                 is private;
   type Message_Type              is private;
   type Message_Block_Length_Type is range <>;
   
   with function Generic_To_Bytes(DWord_Array : Hash_Type) return Bytes is <>;
   
   with procedure Init(Hash_Value : out Hash_Type) is <>;

   with procedure Round(Message_Block : in     Message_Type;
                        Hash_Value    : in out Hash_Type) is <>;

   with function Final_Round(Last_Message_Block  : Message_Type;
                             Last_Message_Length :
                             Message_Block_Length_Type;
                             Hash_Value          : Hash_Type)
                            return Hash_Type is <>;

   with procedure Hash(Message    : in Bytes;
                       Hash_Value : out  Hash_Type) is <>;

   with procedure Hash(Message    : in String;
                       Hash_Value : out  Hash_Type) is <>;

   with procedure F_Hash(Filename : in String;
                         Hash_Value : out  Hash_Type) is <>;
   

package Crypto.Hashfunction is
   function Hash  (Message  : Bytes)  return Hash_Type;
   function Hash  (Message  : String) return Hash_Type;
   function F_Hash(Filename : String) return Hash_Type;
   
   function To_Bytes(Hash : Hash_Type) return Bytes;

   procedure Init;
   procedure Round(Message_Block : in Message_Type);
   function Final_Round(Last_Message_Block  : Message_Type;
                        Last_Message_Length : Message_Block_Length_Type)
                       return Hash_Type;


private
   pragma Inline(Init, Round, Final_Round, Hash, F_Hash);
   pragma Optimize (Time);
end Crypto.Hashfunction;

