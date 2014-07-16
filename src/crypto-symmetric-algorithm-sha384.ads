with Crypto.Symmetric.Algorithm.Sha_Utils;
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

-- This SHA-384 implementation is based on fips-180-2



package Crypto.Symmetric.Algorithm.SHA384 is


   type Generic_Interface is Interface;
   type SHA384_Context is new Generic_Interface with
      record
         Utils_Context : Crypto.Symmetric.Algorithm.Sha_Utils.Sha_Utils_Context;
         Hash_Value : DW_Block512;
      end record;

   -- low level API

   procedure Init(Hash_Value : out DW_Block512);

   procedure Round(Message_Block : in DW_Block1024;
                          Hash_Value : in out DW_Block512);

   function Final_Round(Last_Message_Block  : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024;
                        Hash_Value          : DW_Block512)
                        return DW_Block384;


   -- low level API with object
   procedure Init(This 		: in out SHA384_Context);

   procedure Round(This 	: in out 	SHA384_Context;
                   Message_Block: in 		DW_Block1024);

   function Final_Round(This 		    : in out SHA384_Context;
                        Last_Message_Block  : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024)
                        return DW_Block384;

   -- high level API

   procedure Hash(Message : in Bytes;  Hash_Value :  out DW_Block384);

   procedure Hash(Message : in String; Hash_Value :  out DW_Block384);

   procedure F_Hash(Filename : in String; Hash_Value :  out DW_Block384);


   ---------------------------------------------------------------------------
   -----------------------------PRIVATE PART----------------------------------
   ---------------------------------------------------------------------------

private

   pragma Inline (Init, Round, Final_Round, Hash);
   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.SHA384;
