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


package  Crypto.Symmetric.algorithm.Sha_Utils is

   -- padding a 512-bit message block.
   -- Input: M := 512-bit Messageblock filled with "0"
   --        L := Length oft the Message in the Messageblock < 512
   -- Output: M := the padded Message
   --         MP :=  if  Is_zero(MP) = False then a "padding-overflow" occurse
   -- Expample:
   -- 1. M contains a 510-bit message. The padded message length is 600-bit.
   --    Then M contaies the first 512 bit of the message and MP the last
   --   78-bit.  MP is filled up with "0"
   --
   -- 2. M contains a 24-bit message. The padded message length is 56-bit
   --    Then M containes the padded message (56-bit) and Is_Zero(MP) = true.

   procedure Padding512(Message_Block  : in out W_Block512;
                        Message_Length : in Message_Length64;
                        MP : out W_Block512);
   -- see padding512
   -- differences to padding 512: sha-utils knows the Mssage_Length.
   procedure Padding1024(Message_Block : in out DW_Block1024;
                         MP            : out DW_Block1024);

   -- Round for SHA384 and SHA512
   procedure Round_SHA2(Message_Block : in DW_Block1024;
                        Hash_Value    : in out DW_Block512);

   -- Final round for  SHA384 and SHA512 (function because SHA384)
   function Final_Round_SHA2(Message_Block        : DW_Block1024;
                             Message_Block_Length : Message_Block_Length1024;
                             Hash_Value           : DW_Block512)
                            return DW_Block512;

   -- Inital Procedure for SHA384 and SHA512
   procedure Init_SHA2;


   ---------------------------------------------------------------------------
   -------------------------------EXCEPTIONS----------------------------------
   ---------------------------------------------------------------------------

   -- The following exception occure when message length >= 2**128-2024;
   SHA2_Constraint_Error : exception;

   ---------------------------------------------------------------------------
   -------------------------------PRIVATE-------------------------------------
   ---------------------------------------------------------------------------


private
   pragma Inline (Init_SHA2);
   pragma Optimize (Time);

end  Crypto.Symmetric.algorithm.Sha_Utils;

