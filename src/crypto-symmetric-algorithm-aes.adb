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

with Crypto.Symmetric.Algorithm.Aes.Tables;
use  Crypto.Symmetric.Algorithm.Aes.Tables;

package body Crypto.Symmetric.Algorithm.AES is


   ---------------------------------------------------------------------------
   --------------------------BUILD_ENCRYPT_KEY--------------------------------
   ---------------------------------------------------------------------------

   procedure Build_Encrypt_Key(Key : in Bytes;
                               Encrypt_Key : in out Words;
                               Nk : in Positive) is
      Temp : Word;

   begin
      -- copy user material bytes into temporary ints
      for I in 0..Nk-1 loop
         Encrypt_Key(I) := To_Word(Key(Nb*i),   Key(Nb*i+1),
                                              Key(Nb*i+2), Key(Nb*i+3));
      end loop;
      for I in Nk..Encrypt_Key'Length-1 loop
         Temp  := Encrypt_Key(I-1);

         if (I mod Nk) = 0 then
            Temp := To_Word(S(Byte1(Temp)), S(Byte2(Temp)),
                                  S(Byte3(Temp)), S(Byte0(Temp)))
              xor Rcon(I / Nk );

         elsif (Nk > 6) and ((I mod Nk) = Nb) then
            Temp:=To_Word(S(Byte0(Temp)), S(Byte1(Temp)),
                                S(Byte2(Temp)), S(Byte3(Temp)));
         end if;

         Encrypt_Key(I) := Encrypt_Key(I-Nk) xor Temp;

      end loop;
   end Build_Encrypt_Key;

   -------------------------------------------------------------------------
   -------------------------BUILD_DECRYPT_KEY-------------------------------
   -------------------------------------------------------------------------

   procedure Build_Decrypt_Key(Encrypt_Key : in Words;
                               Decrypt_Key : out Words) is

      Rounds : constant Positive := Encrypt_Key'Length/Nb-1;
      A0, A1, A2, A3 : Word; -- Working variables
      Index : Natural:=Nb; -- current Roundkey index
   begin

      Decrypt_Key := Encrypt_Key;

      for R in 1..Rounds-1 loop -- R*Nb
         A0 := Decrypt_Key(Index); -- Index = R*Nb
         Index:=Index+1;
         A1 := Decrypt_Key(Index); -- Index = R*Nb+1
         Index:=Index+1;
         A2 := Decrypt_Key(Index); -- Index = R*Nb+2
         Index:=Index+1;
         A3 := Decrypt_Key(Index); -- Index := R*Nb+3

         Index:=Index-3; -- Index := R*Nb;

         Decrypt_Key(Index) := U1(Byte0(A0)) xor U2(Byte1(A0)) xor
           U3(Byte2(A0)) xor U4(Byte3(A0));
         Index:= Index+1; -- Index := R*Nb+1;
         Decrypt_Key(Index) := U1(Byte0(A1)) xor U2(Byte1(A1)) xor
           U3(Byte2(A1)) xor U4(Byte3(A1));
         Index:= Index+1; -- Index := R*Nb+2;
         Decrypt_Key(Index) := U1(Byte0(A2)) xor U2(Byte1(A2)) xor
           U3(Byte2(A2)) xor U4(Byte3(A2));
         Index:= Index+1; -- Index := R*Nb+3;
         Decrypt_Key(Index) := U1(Byte0(A3)) xor U2(Byte1(A3)) xor
           U3(Byte2(A3)) xor U4(Byte3(A3));
         Index:= Index+1; -- Index := (R+1)*Nb;
      end loop;

   end Build_Decrypt_KEY;

   -------------------------------------------------------------------------
   ---------------------------PREPARE_KEY-----------------------------------
   -------------------------------------------------------------------------

   procedure Prepare_Key128(Key       : in B_Block128;
                             Cipherkey : out Cipherkey_AES128) is
   begin
      Build_Encrypt_Key(Bytes(Key), Words(Cipherkey.Encrypt_Key), Nk=> Nk128);
      Build_Decrypt_Key(Words(Cipherkey.Encrypt_Key),
                        Words(Cipherkey.Decrypt_Key));
   end Prepare_Key128;

   ---------------------------------------------------------------------------

   procedure Prepare_Key192(Key       : in B_Block192;
                             Cipherkey : out Cipherkey_AES192) is
   begin
      Build_Encrypt_Key(Bytes(Key), Words(Cipherkey.Encrypt_Key), Nk=> Nk192);
      Build_Decrypt_Key(Words(Cipherkey.Encrypt_Key),
                        Words(Cipherkey.Decrypt_Key));
   end Prepare_Key192;

   ---------------------------------------------------------------------------

   procedure Prepare_Key256(Key : in B_Block256;
                            Cipherkey : out Cipherkey_AES256) is
   begin
      Build_Encrypt_Key(Bytes(Key), Words(Cipherkey.Encrypt_Key),
                              Nk=> Nk256);
      Build_Decrypt_Key(Words(Cipherkey.Encrypt_Key),
                        Words(Cipherkey.Decrypt_Key));
   end Prepare_Key256;

   -------------------------------------------------------------------------
   ------------------------------ENCRYPT------------------------------------
   -------------------------------------------------------------------------

   function Encrypt(Encrypt_Key : in Words; Plaintext : in B_Block128)
                   return  B_Block128 is
      X0, X1, X2, X3  : Word;
      A0, A1, A2, A3  : Word;
      Index : Natural := Nb; -- current Encrypt_Key index
      Ciphertext      : B_Block128;
      Rounds          : constant  Positive:= Encrypt_Key'Length/Nb-1;
   begin

      X0 := To_Word(Plaintext(0), Plaintext(1), Plaintext(2),
                          Plaintext(3)) xor Encrypt_Key(0);
      X1 := To_Word(Plaintext(4), Plaintext(5), Plaintext(6),
                          Plaintext(7)) xor Encrypt_Key(1);
      X2 := To_Word(Plaintext(8), Plaintext(9), Plaintext(10),
                          Plaintext(11)) xor Encrypt_Key(2);
      X3 := To_Word(Plaintext(12), Plaintext(13), Plaintext(14),
                          Plaintext(15)) xor Encrypt_Key(3);



      -- AES with table lookup ("The Rijndael Block Cipher",
      -- AES Proposal, Document version 2, Date:03/09/99,  page 18)
      for R in 1..Rounds-1 loop
         A0 := (T1(Byte0(X0))  xor T2(Byte1(X1)) xor T3(Byte2(X2)) xor
                T4(Byte3(X3))) xor Encrypt_Key(Index);
         Index := Index+1;
         A1 := (T1(Byte0(X1))  xor T2(Byte1(X2)) xor T3(Byte2(X3)) xor
                T4(Byte3(X0))) xor Encrypt_Key(Index);
         Index := Index+1;
         A2 := (T1(Byte0(X2))  xor T2(Byte1(X3)) xor T3(Byte2(X0)) xor
                T4(Byte3(X1))) xor Encrypt_Key(Index);
         Index := Index+1;
         A3 := (T1(Byte0(X3))  xor T2(Byte1(X0)) xor T3(Byte2(X1)) xor
                T4(Byte3(X2))) xor Encrypt_Key(Index);

         Index := Index+1; -- Index := (R+1)*Nb;

         X0 := A0;
         X1 := A1;
         X2 := A2;
         X3 := A3;

      end loop;

      -- Index = Rounds*Nb

        -- last round is special
      Ciphertext(0)  := S(Byte0(X0)) xor Byte0(Encrypt_Key(Index));
      Ciphertext(1)  := S(Byte1(X1)) xor Byte1(Encrypt_Key(Index));
      Ciphertext(2)  := S(Byte2(X2)) xor Byte2(Encrypt_Key(Index));
      Ciphertext(3)  := S(Byte3(X3)) xor Byte3(Encrypt_Key(Index));

      Index := Index+1;

      Ciphertext(4)  := S(Byte0(X1)) xor Byte0(Encrypt_Key(Index));
      Ciphertext(5)  := S(Byte1(X2)) xor Byte1(Encrypt_Key(Index));
      Ciphertext(6)  := S(Byte2(X3)) xor Byte2(Encrypt_Key(Index));
      Ciphertext(7)  := S(Byte3(X0)) xor Byte3(Encrypt_Key(Index));

      Index := Index+1;

      Ciphertext(8)  := S(Byte0(X2)) xor Byte0(Encrypt_Key(Index));
      Ciphertext(9)  := S(Byte1(X3)) xor Byte1(Encrypt_Key(Index));
      Ciphertext(10) := S(Byte2(X0)) xor Byte2(Encrypt_Key(Index));
      Ciphertext(11) := S(Byte3(X1)) xor Byte3(Encrypt_Key(Index));

      Index := Index+1;

      Ciphertext(12) := S(Byte0(X3)) xor Byte0(Encrypt_Key(Index));
      Ciphertext(13) := S(Byte1(X0)) xor Byte1(Encrypt_Key(Index));
      Ciphertext(14) := S(Byte2(X1)) xor Byte2(Encrypt_Key(Index));
      Ciphertext(15) := S(Byte3(X2)) xor Byte3(Encrypt_Key(Index));

      return Ciphertext;
   end Encrypt;

   function Encrypt_4rounds(Encrypt_Key : in Words; Plaintext : in B_Block128)
                   return  B_Block128 is
      X0, X1, X2, X3  : Word;
      A0, A1, A2, A3  : Word;
      Index : Natural := Nb; -- current Encrypt_Key index
      Ciphertext      : B_Block128;
      Rounds          : constant  Positive:= 4;
   begin

      X0 := To_Word(Plaintext(0), Plaintext(1), Plaintext(2),
                          Plaintext(3)) xor Encrypt_Key(0);
      X1 := To_Word(Plaintext(4), Plaintext(5), Plaintext(6),
                          Plaintext(7)) xor Encrypt_Key(1);
      X2 := To_Word(Plaintext(8), Plaintext(9), Plaintext(10),
                          Plaintext(11)) xor Encrypt_Key(2);
      X3 := To_Word(Plaintext(12), Plaintext(13), Plaintext(14),
                          Plaintext(15)) xor Encrypt_Key(3);



      -- AES with table lookup ("The Rijndael Block Cipher",
      -- AES Proposal, Document vwersion 2, Date:03/09/99,  page 18)
      for R in 1..Rounds loop
         A0 := (T1(Byte0(X0))  xor T2(Byte1(X1)) xor T3(Byte2(X2)) xor
                T4(Byte3(X3))) xor Encrypt_Key(Index);
         Index := Index+1;
         A1 := (T1(Byte0(X1))  xor T2(Byte1(X2)) xor T3(Byte2(X3)) xor
                T4(Byte3(X0))) xor Encrypt_Key(Index);
         Index := Index+1;
         A2 := (T1(Byte0(X2))  xor T2(Byte1(X3)) xor T3(Byte2(X0)) xor
                T4(Byte3(X1))) xor Encrypt_Key(Index);
         Index := Index+1;
         A3 := (T1(Byte0(X3))  xor T2(Byte1(X0)) xor T3(Byte2(X1)) xor
                T4(Byte3(X2))) xor Encrypt_Key(Index);

         Index := Index+1; -- Index := (R+1)*Nb;

         X0 := A0;
         X1 := A1;
         X2 := A2;
         X3 := A3;

      end loop;

      -- Index = Rounds*Nb

        -- last round is ommitted
      Ciphertext(0..3):=Byte0(A0)&Byte1(A0)&Byte2(A0)&Byte3(A0);
      Ciphertext(4..7):=Byte0(A1)&Byte1(A1)&Byte2(A1)&Byte3(A1);
      Ciphertext(8..11):=Byte0(A2)&Byte1(A2)&Byte2(A2)&Byte3(A2);
      Ciphertext(12..15):=Byte0(A3)&Byte1(A3)&Byte2(A3)&Byte3(A3);

      return Ciphertext;
   end Encrypt_4rounds;

   -------------------------------------------------------------------------
   -------------------------------------------------------------------------

   procedure Encrypt128(Cipherkey  : in  Cipherkey_AES128;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt(Words(Cipherkey.Encrypt_Key), Plaintext);
   end Encrypt128;

   -------------------------------------------------------------------------

   procedure Encrypt128_4rounds(Cipherkey  : in  Cipherkey_AES128;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt_4rounds(Words(Cipherkey.Encrypt_Key), Plaintext);
   end Encrypt128_4rounds;

   -------------------------------------------------------------------------

   procedure Encrypt192(Cipherkey  : in  Cipherkey_AES192;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt(Words(Cipherkey.Encrypt_Key), Plaintext);
   end Encrypt192;

   -------------------------------------------------------------------------

   procedure Encrypt256(Cipherkey  : in  Cipherkey_AES256;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt(Words(Cipherkey.Encrypt_Key), Plaintext);
   end Encrypt256;


   -------------------------------------------------------------------------
   --------------------------DECRYPT----------------------------------------
   -------------------------------------------------------------------------

   function Decrypt(Decrypt_Key : in Words; Ciphertext : B_Block128)
                   return B_Block128 is
      Rounds : constant Positive := Decrypt_Key'Length/Nb-1;
      Index : Natural:=Rounds*Nb;         -- current Decrypt_Key index
      A0, A1, A2, A3 : Word;   -- working variables
      X0, X1, X2, X3 : Word;   -- working variables
      Plaintext : B_Block128;
   begin

      -- ciphertext To B_Block + key
      X0 := To_Word(Ciphertext(0), Ciphertext(1), Ciphertext(2),
                          Ciphertext(3))  xor Decrypt_Key(Index);
      Index:= Index+1; -- Index := Rounds*Nb+1;
      X1 := To_Word(Ciphertext(4), Ciphertext(5), Ciphertext(6),
                          Ciphertext(7))   xor Decrypt_Key(Index);
      Index:= Index+1; -- Index := Rounds*Nb+2;
      X2 := To_Word(Ciphertext(8), Ciphertext(9), Ciphertext(10),
                          Ciphertext(11)) xor Decrypt_Key(Index);
      Index:= Index+1; -- Index := Rounds*Nb+3;
      X3 := To_Word(Ciphertext(12), Ciphertext(13), Ciphertext(14),
                          Ciphertext(15)) xor Decrypt_Key(Index);


      for R in 1..Rounds-1 loop
         Index:=(Rounds-R)*Nb;

         A0 := (T5(Byte0(X0))  xor T6(Byte1(X3)) xor T7(Byte2(X2)) xor
                T8(Byte3(X1))) xor Decrypt_Key(Index);

         Index:= Index+1;

         A1 := (T5(Byte0(X1))  xor T6(Byte1(X0)) xor T7(Byte2(X3)) xor
                T8(Byte3(X2))) xor Decrypt_Key(Index);

         Index:= Index+1;

         A2 := (T5(Byte0(X2))  xor T6(Byte1(X1)) xor T7(Byte2(X0)) xor
                T8(Byte3(X3))) xor Decrypt_Key(Index);

         Index:= Index+1;

         A3 := (T5(Byte0(X3))  xor T6(Byte1(X2)) xor T7(Byte2(X1)) xor
                T8(Byte3(X0))) xor Decrypt_Key(Index);

         X0 := A0;
         X1 := A1;
         X2 := A2;
         X3 := A3;

      end loop;

      -- last round is special
        Plaintext(0)  := Si(Byte0(X0)) xor Byte0(Decrypt_Key(0));
        Plaintext(1)  := Si(Byte1(X3)) xor Byte1(Decrypt_Key(0));
        Plaintext(2)  := Si(Byte2(X2)) xor Byte2(Decrypt_Key(0));
        Plaintext(3)  := Si(Byte3(X1)) xor Byte3(Decrypt_Key(0));

        Plaintext(4)  := Si(Byte0(X1)) xor Byte0(Decrypt_Key(1));
        Plaintext(5)  := Si(Byte1(X0)) xor Byte1(Decrypt_Key(1));
        Plaintext(6)  := Si(Byte2(X3)) xor Byte2(Decrypt_Key(1));
        Plaintext(7)  := Si(Byte3(X2)) xor Byte3(Decrypt_Key(1));

        Plaintext(8)  := Si(Byte0(X2)) xor Byte0(Decrypt_Key(2));
        Plaintext(9)  := Si(Byte1(X1)) xor Byte1(Decrypt_Key(2));
        Plaintext(10) := Si(Byte2(X0)) xor Byte2(Decrypt_Key(2));
        Plaintext(11) := Si(Byte3(X3)) xor Byte3(Decrypt_Key(2));

        Plaintext(12) := Si(Byte0(X3)) xor Byte0(Decrypt_Key(3));
        Plaintext(13) := Si(Byte1(X2)) xor Byte1(Decrypt_Key(3));
        Plaintext(14) := Si(Byte2(X1)) xor Byte2(Decrypt_Key(3));
        Plaintext(15) := Si(Byte3(X0)) xor Byte3(Decrypt_Key(3));

      return Plaintext;
   end Decrypt;

   -------------------------------------------------------------------------
   -------------------------------------------------------------------------

   procedure Decrypt128(Cipherkey  : in  Cipherkey_AES128;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Plaintext := Decrypt(Words(Cipherkey.Decrypt_Key), Ciphertext);
   end Decrypt128;

   -------------------------------------------------------------------------

   procedure Decrypt192(Cipherkey  : in  Cipherkey_AES192;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Plaintext := Decrypt(Words(Cipherkey.Decrypt_Key), Ciphertext);
   end Decrypt192;

   -------------------------------------------------------------------------

   procedure Decrypt256(Cipherkey  : in  Cipherkey_AES256;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Plaintext := Decrypt(Words(Cipherkey.Decrypt_Key), Ciphertext);
   end Decrypt256;

end Crypto.Symmetric.Algorithm.AES;
