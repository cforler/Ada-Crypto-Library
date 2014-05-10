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

-- This SHA-1 implementation is based on fips-180-2

package Crypto.Symmetric.Algorithm.SHA1 is

   subtype Sha1_Type is Natural range 0..79;

   type Generic_Interface is Interface;
   type SHA1_Context is new Generic_Interface with
      record
         Utils_Context : Crypto.Symmetric.Algorithm.Sha_Utils.Sha_Utils_Context;
         Hash_Value : W_Block160;
         Current_Message_Length : Message_Length64;
      end record;
   
   ---------------------------------------------------------------------------
   
   -- low level API with object
   procedure Init(This 		: in out SHA1_Context);

   procedure Round(This 	: in out 	SHA1_Context;
                   Message_Block: in 		W_Block512);

   function Final_Round(This 		    : in out SHA1_Context;
                        Last_Message_Block  : W_Block512;
                        Last_Message_Length : Message_Block_Length512)
                        return W_Block160;
   
   ---------------------------------------------------------------------------

   -- low level API

   procedure Init(Hash_Value : out W_Block160);

   procedure Round(Message_Block : in     W_Block512;
                   Hash_Value    : in out W_Block160);
   
   
   --  Last_Message_Length is the length of the last message block in bytes.
   function Final_Round(Last_Message_Block  : W_Block512;
                        Last_Message_Length : Message_Block_Length512;
                        Hash_Value          : W_Block160)
                       return W_Block160;

   ---------------------------------------------------------------------------

      -- high level API

   procedure Hash(Message : in String; Hash_Value : out W_Block160);

   procedure Hash(Message : in Bytes;  Hash_Value : out W_Block160);

   procedure F_Hash(Filename : in String; Hash_Value : out W_Block160);


   ---------------------------------------------------------------------------

   -- The following exception occure when: message length >= 2**64-1024;
   SHA1_Constraint_Error : Exception;

   ---------------------------------------------------------------------------
   -----------------------------PRIVATE PART----------------------------------
   ---------------------------------------------------------------------------

private
   pragma Inline (Init);
   pragma Inline (Hash);
   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm.SHA1;

