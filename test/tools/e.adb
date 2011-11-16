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
with Crypto.Symmetric.Blockcipher_TripleDES;
with Crypto.Symmetric.Mode.CBC;
with Crypto.Symmetric.Mode.OFB;
with Crypto.Symmetric.Mode.CFB;
with Crypto.Symmetric.Mode.CTR;

with Ada.Text_IO; use Ada.Text_IO;

procedure E is

   package DIO is new Ada.Text_Io.Modular_IO (DWord);

   package TDES renames Crypto.Symmetric.Blockcipher_TripleDES;
   package TDES_CBC is new Crypto.Symmetric.Mode.CBC(TDES);
   package TDES_OFB is new Crypto.Symmetric.Mode.OFB(TDES);
   package TDES_CFB is new Crypto.Symmetric.Mode.CFB(TDES);
   package TDES_CTR is new Crypto.Symmetric.Mode.CTR(TDES);

   use TDES_CBC;

   TDES_KEY : B_Block192 := (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                             16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                             16#00#, 16#00#, 16#00#, 16#00#, 16#A0#, 16#23#,
                             16#15#, 16#67#, 16#89#, 16#ab#, 16#ad#, 16#ef#);

   IV_TDES : B_Block64 :=
     (16#12#, 16#34#, 16#56#, 16#78#, 16#90#, 16#ab#, 16#cd#, 16#ef#);

   P_TDES_String : String :="All your base are belong to us!";
   P_TDES : array (1..3) of B_Block64 :=
     ((To_Bytes(P_TDES_String(1..8))),
      (To_Bytes(P_TDES_String(9..16))),
      (To_Bytes(P_TDES_String(17..24))));

   C_TDES : array (0..3) of B_Block64;

begin

    --Initialisierung
    Init(TDES_KEY, IV_TDES);

    -- 1. Chiffreblock = Startwert.
    C_TDES(0) := IV_TDES;

   --Verschluesselung
   for I in P_TDES'Range loop
      Encrypt(P_TDES(I), C_TDES(I));
   end loop;

   -- Fuer die Entschluesselung wird die Chiffre mit dem
   -- gleichen Startwert wie bei der Entschluesselung reinitalisiert
     Set_IV(C_TDES(0));

   for I in P_TDES'Range loop
      P_TDES(I) := (others => 0);
      Decrypt(C_TDES(I), P_TDES(I));
       Put(To_String(P_TDES(I)));
   end loop;
end E;
