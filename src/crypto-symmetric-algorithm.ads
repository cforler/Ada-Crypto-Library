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

--with Ada.IO_Exceptions; use Ada.IO_Exceptions;
--with Gnat.Os_Lib; use Gnat.Os_Lib;
with Crypto.Types; use Crypto.Types;

package Crypto.Symmetric.Algorithm is

   type Message_Length64  is private;
   type Message_Length128 is private;
   type Message_Length256 is private;

    ---------------------------------------------------------------------------
   ----------------------------EXCEPTIONS------------------------------------
   ---------------------------------------------------------------------------

   File_Open_Error : exception;
   File_Read_Error : exception;

   ---------------------------------------------------------------------------
   ------------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------


   private
      type Direction_type is (Encrypt, Decrypt);

          -- length of message must  < 2^64
   type Message_Length64 is new DWord;

   -- length of message must < 2^128
   type Message_Length128 is record
      M1 : DWord;
      M2 : Dword;
   end record;

   type Message_Length256 is new  DWords(0..3);

   type Message_Counter_Type is mod 16;
   type To_Word_Counter_Type is mod 4;
   type To_DWord_Counter_Type is mod 8;

   pragma Optimize (Time);

end Crypto.Symmetric.Algorithm;

