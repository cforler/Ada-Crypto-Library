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

with  Crypto.Symmetric.Algorithm.Tripledes.Tables;
use   Crypto.Symmetric.Algorithm.Tripledes.Tables;

--with Ada.Text_IO; use Ada.Text_IO;

package body Crypto.Symmetric.Algorithm.Tripledes is


--   package BIO is new Ada.Text_Io.Modular_IO (Byte);
--   package WIO is new Ada.Text_Io.Modular_IO (Word);

   ---------------------------------------------------------------------------
   ----------------------------PREPARE_KEY------------------------------------
   ---------------------------------------------------------------------------

   procedure Cookey(Kn : in out  Roundkey_DES) is
      Raw1  : constant Roundkey_DES := kn;
      Dough : Roundkey_DES;
      C, J, K  : Integer:=0;
   begin
      for I in 0..15 loop
         K:=J;
         J:=J+1;
         Dough(C) := Shift_Left(Raw1(K) and 16#00fc0000#,6);
         Dough(C) := Dough(C) or Shift_Left(Raw1(K) and 16#00000fc0#, 10);
         Dough(C) := Dough(C) or Shift_Right(Raw1(J) and 16#00fc0000#, 10);
         Dough(C) := Dough(C) or Shift_Right(Raw1(J) and 16#00000fc0#,  6);
         C:=C+1;
         Dough(C) := Shift_Left(Raw1(K) and 16#0003f000#,12);
         Dough(C) := Dough(C) or Shift_left(Raw1(K) and 16#0000003f#, 16);
         Dough(C) := Dough(C) or Shift_Right(Raw1(J) and 16#0003f000#,  4);
         Dough(C) := Dough(C) or (Raw1(J) and  16#0000003f#);
         C:=C+1;
         J:=J+1;
      end loop;

      Kn := Dough;

      -- wipeout key material
      Dough:=( others => 0);

   end Cookey;

   ---------------------------------------------------------------------------


   procedure Build_Roundkey(Key : in B_Block64;
                            kn : out Roundkey_DES;
                            Direction : in Direction_type) is
      L, M, N : Word;
      Pc1m, pcr : array(0..55) of Byte;
   begin
      for J in 0..55 loop
         L:=Word(Pc1(J));
         M:= L and 8#7#;
         if (Key(Integer(shift_Right(L,3))) and  Bytebit(Integer(M))) /= 0
         then Pc1m(J):=1;
         else Pc1m(J):=0;
         end if;
      end loop;

      for I in 0..15 loop
         if (Direction = Decrypt) then
            M := Shift_Left(Word(15 - I),1);
         else
            M := Shift_Left(Word(I),1);
         end if;
         N :=M+1;
         Kn(Natural(M)) := 0;
         Kn(Natural(n)) := 0;

         for j in 0..27 loop
            L := Word(J)+Totrot(I);
            if L < 28 then
               Pcr(J):= Pc1m(Integer(L));
            else
               Pcr(J):=Pc1m(Integer(L)-28);
            end if;
         end loop;

         for J in 28..55 loop
            L := Word(J) + Totrot(I);
            if L < 56 then
                  Pcr(J):= Pc1m(Integer(L));
            else
               Pcr(J) := Pc1m(Integer(L)-28);
            end if;
         end loop;

         for J in 0..23 loop
            if Pcr(Pc2(J)) /= 0 then
               Kn(Natural(M)) := Kn(Natural(M)) or Bigbyte(J);
            end if;

            if Pcr(Pc2(J+24)) /= 0 then
               Kn(Natural(N)) := Kn(Natural(N)) or Bigbyte(J);
            end if;

         end loop;
      end loop;
      Cookey(Kn);
   end Build_Roundkey;

   ---------------------------------------------------------------------------

   procedure Build_Encrypt_Key(Key : in B_Block64; Kn : out Roundkey_DES) is
      Direction : constant Direction_Type := Encrypt;
   begin
      Build_Roundkey(Key, Kn, Direction);
   end Build_Encrypt_Key; pragma Inline(Build_Encrypt_Key);

   ---------------------------------------------------------------------------


   procedure Build_Decrypt_Key(Key : in B_Block64; Kn : out Roundkey_DES) is
      Direction : constant Direction_Type := Decrypt;
   begin
      Build_Roundkey(Key, Kn, Direction);
   end Build_Decrypt_Key; pragma Inline(Build_Decrypt_Key);

   ---------------------------------------------------------------------------
   --------------------------DES_FUNCTION-------------------------------------
   ---------------------------------------------------------------------------


   function Des(Roundkey : in Roundkey_DES; Input : in B_Block64)
               return B_Block64 is
      Output : Bytes(B_Block64'Range);
      Fval, W : Word;
      Left : Word := To_Word(Input(0),Input(1),
                               Input(2),Input(3));
      Right : Word := To_Word(Input(4),Input(5),
                               Input(6),Input(7));
      J : Natural:=0;
   begin

      -- Initial permutation

      W:= (Shift_Right(Left,4) xor Right) and 16#0f0f0f0f#;
      Right := Right xor W;
      Left  := Left xor Shift_Left(W,4);

      W := (Shift_Right(Left,16) xor Right) and 16#0000ffff#;
      Right := Right xor W;
      Left  := Left xor Shift_Left(W,16);


      W := (Shift_Right(Right,2) xor Left) and 16#33333333#;
      Left  := Left xor W;
      Right := Right xor Shift_Left(W,2);


      W := (Shift_Right(Right,8) xor left) and 16#00ff00ff#;
      Left  := Left xor W;
      Right := Right xor Shift_Left(W,8);


      Right  := (Shift_Left(Right,1) or (Shift_Right(Right,31) and 1))
        and 16#Ffffffff#;
      W := (Left xor Right) and 16#Aaaaaaaa#;
      Left := Left xor w;
      Right :=  Right xor W;
      Left  := (Shift_Left(Left, 1) or (Shift_Right(Left, 31) and 1))
        and 16#Ffffffff#;


      -- 8 DES doublerounds (=16 rounds DES):
      
      for Round in 0..7 loop
         W  := Shift_Left(Right, 28) or Shift_Right(Right, 4);

         -- We need no expansion only a key addition

         W := W xor  Roundkey(J);
         J:=J+1;

         -- SBox transformation (right half):
	 
         Fval := SP7(w and 16#3f#);
         Fval := Fval or SP5(Shift_Right(W,  8) and 16#3f#);
         Fval := Fval or SP3(Shift_Right(W, 16) and 16#3f#);
         Fval := Fval or SP1(Shift_right(W, 24) and 16#3f#);

         W := Right xor Roundkey(J);


         -- SBox transformation (left half)
         J:=J+1;

         Fval := Fval or SP8(W and 16#3f#);
         Fval := Fval or SP6(Shift_Right(W,  8) and 16#3f#);
         Fval := Fval or SP4(Shift_Right(W, 16) and 16#3f#);
         Fval := Fval or SP2(Shift_Right(W, 24) and 16#3f#);

         Left := Left xor Fval;

         -- End of one DES round

         --- Now we start another DES round
         --  So we avoid the problem to permute
         --  the left with the right side.

         W := Shift_left(Left, 28) or Shift_Right(Left, 4);
         W := W xor Roundkey(J);
         J:=J+1;

         Fval := SP7(w and 16#3f#);
         Fval := Fval or SP5(Shift_Right(W,  8) and 16#3f#);
         Fval := Fval or SP3(Shift_Right(W, 16) and 16#3f#);
         Fval := Fval or SP1(Shift_Right(W, 24) and 16#3f#);
         W := Left xor Roundkey(J);
         J:= J+1;
         Fval := Fval or SP8(w and 16#3f#);
         Fval := Fval or SP6(Shift_Right(W,  8) and 16#3f#);
         Fval := Fval or SP4(Shift_Right(W, 16) and 16#3f#);
         Fval := Fval or SP2(Shift_Right(W, 24) and 16#3f#);
         Right := Right xor Fval;
      end loop;


      --- inverse initiale permutation
      Right := Shift_Left(Right, 31) or Shift_Right(Right,1);
      W     := (Left xor Right) and 16#Aaaaaaaa#;
      Left  := Left xor W;
      Right := Right xor W;
      Left  := Shift_Left(Left, 31) or Shift_Right(Left,1);
      W     := (Shift_Right(Left,8) xor Right) and 16#00ff00ff#;
      Right := Right xor W;
      Left  := Left xor  Shift_Left(W, 8);
      W     := (Shift_Right(Left,2) xor Right) and 16#33333333#;
      Right := Right xor W;
      Left  := Left xor Shift_Left(W,2);
      W     := (Shift_Right(Right, 16) xor Left) and 16#0000ffff#;
      Left  :=  Left xor W;
      Right := Right xor Shift_Left(W, 16);
      W     := (Shift_Right(Right, 4) xor Left) and 16#0f0f0f0f#;
      Left  := Left xor w;
      Right := Right xor Shift_left(W, 4);

      -- unscrun
      Output(0..3):= To_Bytes(Right);
      Output(4..7):= To_Bytes(Left);

      return B_Block64(Output);
   end Des;


   ---------------------------------------------------------------------------
   --------------------------API----------------------------------------------
   ---------------------------------------------------------------------------

   procedure Prepare_Key(Key : in B_Block192;
                         Cipherkey : out Cipherkey_TDES) is
   begin
      -- create the 1st cipherkey
      Build_Encrypt_Key(B_Block64(Key( 0.. 7)), Cipherkey(0).Encrypt_Key);
      Build_Encrypt_Key(B_Block64(Key( 8..15)), Cipherkey(1).Encrypt_Key);
      Build_Encrypt_Key(B_Block64(Key(16..23)), Cipherkey(2).Encrypt_Key);


      Build_Decrypt_Key(B_Block64(Key( 0.. 7)), Cipherkey(0).Decrypt_Key);
      Build_Decrypt_Key(B_Block64(Key( 8..15)), Cipherkey(1).Decrypt_Key);
      Build_Decrypt_Key(B_Block64(Key(16..23)), Cipherkey(2).Decrypt_Key);

   end Prepare_Key;

   ---------------------------------------------------------------------------

   procedure Encrypt(Cipherkey  : in  Cipherkey_TDES;
                     Plaintext  : in  B_Block64;
                     Ciphertext : out B_Block64) is
   begin
      Ciphertext :=Des(Cipherkey(2).Encrypt_Key,
                       Des(Cipherkey(1).Decrypt_Key,
                           Des(Cipherkey(0).Encrypt_Key, Plaintext)));
   end Encrypt;

   ---------------------------------------------------------------------------

   procedure Decrypt(Cipherkey  : in  Cipherkey_TDES;
                     Ciphertext : in  B_Block64;
                     Plaintext  : out B_Block64) is
   begin
      -- 3DES DED-decryption
      Plaintext := Des(Cipherkey(0).Decrypt_Key,
                       Des(Cipherkey(1).Encrypt_Key,
                           Des(Cipherkey(2).Decrypt_Key, Ciphertext)));

   end Decrypt;

end Crypto.Symmetric.Algorithm.Tripledes;

