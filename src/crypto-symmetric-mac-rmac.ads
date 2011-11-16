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

with Crypto.Symmetric.Oneway_Blockcipher;

generic
   with package C is new Crypto.Symmetric.Oneway_Blockcipher(<>);

   with procedure Read (Random : out C.Key_Type) is <>;
   with function "xor" (Left, Right : C.Block)    return C.Block is <>;
   with function "xor" (Left, Right : C.Key_type) return C.Key_Type is <>;

package Crypto.Symmetric.Mac.Rmac is
   use C;

   type Blocks  is array (Integer range <>) of Block;

   -- low level
   procedure Init(Key1, Key2 : in Key_Type);

   procedure Sign(Message_Block : in Block);
   procedure Final_Sign(Final_Message_Block : in Block;
                        R   : out Key_Type;
                        Tag : out Block);


   procedure Verify(Message_Block : in Block);
   function Final_Verify(Final_Message_Block  : in Block;
                         R   : in Key_Type;
                         Tag : in Block)
                        return Boolean;


   -- high level API

   procedure Sign(Message : in Blocks;
                  Key1, Key2 : in Key_Type;
                  R   : out Key_Type;
                  Tag : out Block);



   function Verify(Message : in Blocks;
                   Key1, Key2 : in Key_Type;
                   R   : in Key_Type;
                   Tag : in Block) return Boolean;


private
   pragma Inline (Init, Sign);

   pragma Optimize (Time);


end crypto.Symmetric.Mac.Rmac;
