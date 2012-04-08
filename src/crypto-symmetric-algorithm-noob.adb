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

-------------------------------------------------------------------------------
--
-- Serpent Blockcipher
--
-- Copyright (c) 1998 Markus G. Kuhn <mkuhn@acm.org>. All rights reserved.
--
-- $Id: crypto-symmetric-algorithm-serpent.adb 1.2 Fri, 17 Dec 2004 16:55:38 +0100 shortie $
--
-------------------------------------------------------------------------------
--
-- This implementation is optimized for best execution time by use of
-- function inlining and loop unrolling. It is not intended to be used in
-- applications (such as smartcards) where machine code size matters. Best
-- compiled with highest optimization level activated and all run-time
-- checks supressed.
--
-------------------------------------------------------------------------------

package body Crypto.Symmetric.Algorithm.Noob is

   procedure Left_Rotate(Data	: in out B_Block64)is
      First_Byte : Byte;
   begin
      First_Byte := Data(Data'First);
      for i in (Data'First+1)..Data'Last loop
      	Data(i-1) := Data(i);
      end loop;
      Data(Data'Last) := First_Byte;
   end Left_Rotate;

   procedure Right_Rotate(Data : in out B_Block64) is
      Last_Byte : Byte;
   begin
      Last_Byte := Data(Data'Last);
      for i in Data'First..(Data'Last-1) loop
      	Data(i+1) := Data(i);
      end loop;
      Data(Data'First) := Last_Byte;
   end Right_Rotate;

   procedure Prepare_Key64(Key       : in  B_Block64;
                           Cipherkey : out Cipherkey_Noob64) is
   begin
      Cipherkey := Cipherkey_Noob64(Key);
   end Prepare_Key64;

   procedure Encrypt64(Cipherkey  : in  Cipherkey_Noob64;
                       Plaintext  : in  B_Block64;
                       Ciphertext : out B_Block64) is
   begin
      Ciphertext := (Plaintext xor B_Block64(Cipherkey));
      Left_Rotate(Ciphertext);
   end Encrypt64;

   procedure Decrypt64(Cipherkey  : in  Cipherkey_Noob64;
                        Ciphertext : in  B_Block64;
                       Plaintext  : out B_Block64) is
   begin
      Right_Rotate(Plaintext);
      Plaintext := Ciphertext xor B_Block64(Cipherkey);
   end Decrypt64;

end Crypto.Symmetric.Algorithm.Noob;
