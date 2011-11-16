
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
-- Blowfish
--
-- Copyright (c) 2006 Maxim Reznik. All rights reserved.
--
-- $Id:  $
--
-------------------------------------------------------------------------------
--
-- This is the Ada95 implementation of the Blowfish.
-- http://www.schneier.com/blowfish.html
--
-------------------------------------------------------------------------------

package Crypto.Symmetric.Algorithm.Blowfish is


   type Cipherkey_Blowfish128 is private;

   ---------------------------------------------------------------------------

   subtype B_Block is Bytes;  --  expected from 32 bits to 448 bits

   procedure Prepare_Key128 (Key       : in  B_Block128;
                             Cipherkey : out Cipherkey_Blowfish128);

   procedure Encrypt128 (Cipherkey  : in  Cipherkey_Blowfish128;
                         Plaintext  : in  B_Block64;
                         Ciphertext : out B_Block64);

   procedure Decrypt128 (Cipherkey  : in  Cipherkey_Blowfish128;
                         Ciphertext : in  B_Block64;
                         Plaintext  : out B_Block64);

   ---------------------------------------------------------------------------
   -----------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------


private

   Rounds : constant := 16;

   type S_Type is array (0 .. 3, Byte) of Word;
   type P_Type is array (0 .. Rounds + 1) of Word;

   type Cipherkey_Blowfish128 is record
      S : S_Type;
      P : P_Type;
   end record;

   pragma Inline (Prepare_Key128, Encrypt128, Decrypt128);
   pragma Optimize (Time);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.Blowfish;
