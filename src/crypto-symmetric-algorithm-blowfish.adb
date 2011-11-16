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

with Crypto.Symmetric.Algorithm.Blowfish.Tables;

package body Crypto.Symmetric.Algorithm.Blowfish is

   subtype Two_Words is Words (0 .. 1);

   procedure Swap (Pieces : in out Two_Words);
   pragma Inline (Swap);

   function F
     (Key  : in  Cipherkey_Blowfish128;
      S    : in  Word) return Word;
   pragma Inline (F);

   procedure To_Words
     (Pieces :    out Two_Words;
      Block  : in     B_Block64);
   pragma Inline (To_Words);

   ----------------
   -- Decrypt128 --
   ----------------

   procedure Decrypt128
     (Cipherkey  : in  Cipherkey_Blowfish128;
      Ciphertext : in  B_Block64;
      Plaintext  : out B_Block64)
   is
      P : Bytes(B_Block64'Range);
      Pieces : Two_Words;
   begin
      To_Words (Pieces, Ciphertext);

      for J in reverse 2 .. P_Type'Last loop
         Pieces (0) := Pieces (0) xor Cipherkey.P (J);
         Pieces (1) := F (Cipherkey, Pieces (0)) xor Pieces (1);

         Swap (Pieces);
      end loop;

      Swap (Pieces);

      Pieces (1) := Pieces (1) xor Cipherkey.P (1);
      Pieces (0) := Pieces (0) xor Cipherkey.P (0);
      
        
      P(0 .. 3) := To_Bytes (Pieces (0));
      P(4 .. 7) := To_Bytes (Pieces (1));    
      Plaintext := B_Block64(P);
   end Decrypt128;

   ----------------
   -- Encrypt128 --
   ----------------

   procedure Encrypt128
     (Cipherkey  : in  Cipherkey_Blowfish128;
      Plaintext  : in  B_Block64;
      Ciphertext : out B_Block64)
   is
      Pieces : Two_Words;
      C : Bytes(B_Block64'Range);
   begin
      To_Words (Pieces, Plaintext);

      for J in 0 .. Rounds - 1 loop
         Pieces (0) := Pieces (0) xor Cipherkey.P (J);
         Pieces (1) := F (Cipherkey, Pieces (0)) xor Pieces (1);

         Swap (Pieces);
      end loop;

      Swap (Pieces);

      Pieces (1) := Pieces (1) xor Cipherkey.P (Rounds);
      Pieces (0) := Pieces (0) xor Cipherkey.P (Rounds + 1);

      C(0 .. 3) := To_Bytes (Pieces (0));
      C(4 .. 7) := To_Bytes (Pieces (1));
      Ciphertext := B_Block64(C);
   end Encrypt128;

   -------
   -- F --
   -------

   function F
     (Key  : in  Cipherkey_Blowfish128;
      S    : in  Word) return Word
   is
      X : constant Byte_Word := To_Bytes (S);
      Y : Word := Key.S (0, X (0)) + Key.S (1, X (1));
   begin
      Y := Y xor Key.S (2, X (2));
      Y := Y + Key.S (3, X (3));
      return Y;
   end F;

   --------------------
   -- Prepare_Key128 --
   --------------------

   procedure Prepare_Key128
     (Key       : in  B_Block128;
      Cipherkey : out Cipherkey_Blowfish128)
   is
      Part       : Byte_Word;
      Part_Index : Integer := Key'First;
      Last       : Integer;
      Block      : B_Block64 := (others => 0);
      Pieces     : Two_Words;
   begin
      Cipherkey.S := Tables.S_Origin;

      for I in Cipherkey.P'Range loop
         if Part_Index + 3 > Key'Last then
            Last := Key'Last - Part_Index;
            Part (0 .. Last) := Bytes(Key)(Part_Index .. Key'Last);
            Part (Last + 1 .. 3) :=
              Bytes(Key)(Key'First .. Key'First + 2 - Last);
            Part_Index := Key'First + 3 - Last;
         else
            Part := Bytes(Key)(Part_Index .. Part_Index + 3);
            Part_Index := Part_Index + 4;
         end if;

         if Part_Index > Key'Last then
            Part_Index := Part_Index - Key'Length;
         end if;

         Cipherkey.P (I) := Tables.P_Origin (I) xor To_Word (Part);
      end loop;

      for I in 0 .. P_Type'Last / 2 loop
         Encrypt128 (Cipherkey, Block, Block);
         To_Words (Pieces, Block);
         Cipherkey.P (2 * I)     := Pieces (0);
         Cipherkey.P (2 * I + 1) := Pieces (1);
      end loop;

      for I in 0 .. 3 loop
         for J in 0 .. Byte'Last / 2 loop
            Encrypt128 (Cipherkey, Block, Block);
            To_Words (Pieces, Block);
            Cipherkey.S (I, 2 * J)     := Pieces (0);
            Cipherkey.S (I, 2 * J + 1) := Pieces (1);
         end loop;
      end loop;
   end Prepare_Key128;

   ----------
   -- Swap --
   ----------

   procedure Swap (Pieces : in out Two_Words) is
      Temp : constant Word := Pieces (0);
   begin
      Pieces (0) := Pieces (1);
      Pieces (1) := Temp;
   end Swap;

   --------------
   -- To_Words --
   --------------

   procedure To_Words
     (Pieces :    out Two_Words;
      Block  : in     B_Block64)
   is
   begin
      Pieces (0) := To_Word (Bytes(Block (0 .. 3)));
      Pieces (1) := To_Word (Bytes(Block (4 .. 7)));
   end To_Words;

end Crypto.Symmetric.Algorithm.Blowfish;
