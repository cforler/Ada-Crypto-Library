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


package Crypto.Symmetric.Algorithm.Whirlpool is


   type Generic_Interface is Interface;
   type Whirlpool_Interface is new Generic_Interface with
      record
         Hash_Value : DW_Block512;
         Current_Message_Length : Message_Length256;
      end record;

   -- low level API

   procedure Init(Hash_Value : out DW_Block512);

   procedure Round(Message_Block : in     DW_Block512;
                   Hash_Value    : in out DW_Block512);

   function Final_Round(Last_Message_Block : DW_Block512;
                        Last_Message_Length: Message_Block_Length512;
                        Hash_Value         : DW_Block512)
                        return DW_Block512;

      -- low level API with object
   procedure Init(This 		: in out Whirlpool_Interface);

   procedure Round(This 	: in out 	Whirlpool_Interface;
                   Message_Block: in 		DW_Block512);

   function Final_Round(This 		    : in out Whirlpool_Interface;
                        Last_Message_Block  : DW_Block512;
                        Last_Message_Length : Message_Block_Length512)
                        return DW_Block512;

   -- high level API
   procedure Hash  (Message  : in Bytes;  Hash_Value : out DW_Block512);
   procedure Hash  (Message  : in String; Hash_Value : out DW_Block512);
   procedure F_Hash(Filename : in String; Hash_Value : out DW_Block512);


    ---------------------------------------------------------------------------
   -------------------------------EXCEPTIONT-----------------------------------
   ----------------------------------------------------------------------------

   -- The following exception occure when: message length >= 2**256-1024
   Whirlpool_Constraint_Error : Exception;

   ---------------------------------------------------------------------------
   -----------------------------PRIVATE PART----------------------------------
   ---------------------------------------------------------------------------

private
   -- number of rounds
   R : constant Natural := 10;

   pragma Inline (Init, Hash);
   pragma Optimize (Time);

end  Crypto.Symmetric.Algorithm.Whirlpool;
