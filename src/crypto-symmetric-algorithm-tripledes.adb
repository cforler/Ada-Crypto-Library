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
--   package SIO is new Ada.Text_Io.Modular_IO (Bit7);
--   package B6  is new Ada.Text_Io.Modular_IO (Bit6);
--   package XIO is new Ada.Text_Io.Modular_IO (Bit7_Word);
--   package B48 is new Ada.Text_Io.Modular_IO (Bit48);

   ---------------------------------------------------------------------------
   ---------------------------OBSOLETE_CODE-----------------------------------
   ---------------------------------------------------------------------------
  
   type Bit7_Word is mod 2**28; -- 4 * Sev'size = 4*7 = 28
   for Bit7_Word'Size use 28;

   --type Bit7s is  array (Integer range <>) of Bit7;
   type Bit7_Words is array (Integer range <>) of Bit7_Word;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function "xor"(Left, Right : in B6_block48) return B6_Block48 is
   Result : B6_Block48;
   begin
      for I in B6_Block48'First .. B6_Block48'Last loop
         Result(I) := Left(I) xor Right(I);
      end loop;
      return Result;
   end "xor"; Pragma Inline("xor");

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------


   -- expansions function E (Optimized: without lookup)
   -- This implementation of the DES E function uses the structur of the
   -- E function to avoid table lookups

   function E_Function(X : in Word) return B6_Block48 is
      Result : B6_Block48; -- Result(N) become the n-th row of th E table
      R : constant B_Block32 :=  B_Block32(To_Bytes(X));
   begin

      -- set the first bit of the result
      Result(0):= Bit6(R(3) mod 2 );
      Result(1):= Bit6(Shift_Right(R(0), 4) mod 2 );
      Result(2):= Bit6(R(0) mod 2 );
      Result(3):= Bit6(Shift_Right(R(1), 4) mod 2 );
      Result(4):= Bit6(R(1) mod 2 );
      Result(5):= Bit6(Shift_Right(R(2), 4) mod 2 );
      Result(6):= Bit6(R(2) mod 2 );
      Result(7):= Bit6(Shift_Right(R(3), 4) mod 2 );


      -- combine the odd and even rows

      for I in reverse 3..7 loop
         Result(0):= Result(0) * 2 or Bit6(Shift_Right(R(0), I) mod 2 );
         Result(2):= Result(2) * 2 or Bit6(Shift_Right(R(1), I) mod 2 );
         Result(4):= Result(4) * 2 or Bit6(Shift_Right(R(2), I) mod 2 );
         Result(6):= Result(6) * 2 or Bit6(Shift_Right(R(3), I) mod 2 );
      end loop;

      for I in reverse 1..3 loop
         Result(1):= Result(1) * 2 or Bit6(Shift_Right(R(0), I) mod 2 );
         Result(3):= Result(3) * 2 or Bit6(Shift_Right(R(1), I) mod 2 );
         Result(5):= Result(5) * 2 or Bit6(Shift_Right(R(2), I) mod 2 );
         Result(7):= Result(7) * 2 or Bit6(Shift_Right(R(3), I) mod 2 );
      end loop;

      -- This is an optimization to avoid the use less machine instruction:
      -- lsh R(0), 0
      Result(1):= Result(1) * 2 or Bit6(R(0) mod 2 );
      Result(3):= Result(3) * 2 or Bit6(R(1) mod 2 );
      Result(5):= Result(5) * 2 or Bit6(R(2) mod 2 );
      Result(7):= Result(7) * 2 or Bit6(R(3) mod 2 );

      -- the last column of the odd-rows divers and needs a extra treament
      Result(1):= Result(1) * 2 or Bit6(Shift_Right(R(1),7) mod 2);
      Result(3):= Result(3) * 2 or Bit6(Shift_Right(R(2),7) mod 2);
      Result(5):= Result(5) * 2 or Bit6(Shift_Right(R(3),7) mod 2);
      Result(7):= Result(7) * 2 or Bit6(Shift_Right(R(0),7) mod 2);

      return Result;
   end E_Function; pragma Inline(E_Function);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   -- round  function f
   function F_Function(R :in  Word; Roundkey : in B6_Block48) return Word is
      Aux : B6_Block48;
      Temp : Word;
      Result : Word:=0;

   begin
      Aux := E_Function(R) xor Roundkey;

      -- S-Box
      Temp :=
        Shift_Left(Es1(Word(Aux(0))),28) xor
        Shift_Left(Es2(Word(Aux(1))),24) xor
        Shift_Left(Es3(Word(Aux(2))),20) xor
        Shift_Left(Es4(Word(Aux(3))),16) xor
        Shift_Left(Es5(Word(Aux(4))),12) xor
        Shift_Left(Es6(Word(Aux(5))), 8) xor
        Shift_Left(Es7(Word(Aux(6))), 4) xor Es8(Word(Aux(7)));


      -- Permutation with the reverse bit order
      for I in 0..31 loop
         Result := Shift_Left(Result,1)  xor  (Shift_Right(Temp, RP(I)) mod 2);
      end loop;

      return Result;
   end F_Function; pragma Inline(F_Function);


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   -- case 1
   -- Input : encryption roundkey
   -- Output: decryption roundkey

   --case 2
   -- Input:  decryption roundkey
   -- Output: encryption roundkey

   procedure Obsolete_Build_Decrypt_Key(Encrypt_Key : in  Obsolete_Roundkey_DES;
                                         Decrypt_Key : out Obsolete_Roundkey_DES) is
   begin
      for I in Obsolete_Roundkey_Des'First .. Obsolete_Roundkey_Des'Last loop
         Decrypt_Key(I) := Encrypt_Key(Obsolete_Roundkey_Des'Last-I);
      end loop;

   end Obsolete_Build_Decrypt_Key; pragma Inline(Obsolete_Build_Decrypt_Key);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Obsolete_Build_Encrypt_Key(Key : in B_Block64; Encrypt_Key : out Obsolete_Roundkey_DES) is
      C : Bit7_Words(0..15):=(others=>0);
      D : Bit7_Words(0..15):=(others=>0);
      Roundkey : Obsolete_Roundkey_DES;
   begin

      -- initialisierung
      Roundkey:=(others=> (others=>0) );

      -- Permutation Choice 1

      for I in 0..27 loop
         C(15) := C(15) or Bit7_Word(Shift_Left(Word((Shift_Right(Key(Pc1key(I)), Pc1rs(I)) mod 2)), 27 - I  ));
      end loop;

      for I in 28..55 loop
         D(15) := D(15) or Bit7_Word(Shift_Left(Word((Shift_Right(Key(Pc1key(I)), Pc1rs(I)) mod 2)), 55 - I ));
      end loop;

      -- With C(15) and D(15) defined, we now create fifteen blocks C(n) and D(n), 0<=n<=14

      C(0) := Bit7_Word(((Shift_Left(Word(C(15)),ls(0)) or Shift_Right(Word(C(15)),lefts(0))) mod 2**28));
      D(0) := Bit7_Word(((Shift_Left(Word(D(15)),ls(0)) or Shift_Right(Word(D(15)),lefts(0))) mod 2**28));

      for I in 1..14 loop
         C(I) := Bit7_Word(((Shift_Left(Word(C(I-1)),ls(I)) or Shift_Right(Word(C(I-1)),lefts(I))) mod 2**28));
         D(I) := Bit7_Word(((Shift_Left(Word(D(I-1)),ls(I)) or Shift_Right(Word(D(I-1)),lefts(I))) mod 2**28));
      end loop;


      -- Permutation Choice 2
      for J in 0..15 loop
         -- Permutation Choice 2 C(I)
         for I in 0..23 loop
            Roundkey(J)(I/6) := Roundkey(J)(I/6) * 2 or Bit6(Shift_Right(Word(C(J)),27-Pc2(I)) mod 2);
         end loop;

         for I in 24..47 loop
            Roundkey(J)(I/6) := Roundkey(J)(I/6) * 2 or Bit6(Shift_Right(Word(D(J)),27-(Pc2(I) mod 28) ) mod 2);
         end loop;
      end loop;

      Encrypt_Key := Roundkey;

   end Obsolete_Build_Encrypt_Key;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------


   function Obsolete_Des(Roundkey : in Obsolete_Roundkey_Des;
                          Plaintext : in B_Block64)
                        return B_Block64 is
      Ciphertext : B_Block64:=(others=>0);
      L, R : Word;
      R_Old : Word;
   begin
      -- initial permutation IP
      for I in  0..7 loop
         for J in reverse 0..7 loop
            Ciphertext(I) := Shift_Left(Ciphertext(I),1) or
              (Shift_Right(Plaintext(J),Ipc(I)) mod 2);
         end loop;
      end loop;

      -- Lets start the 16 "Feistel-Rounds"

      --Setup
      L     := To_Word(Ciphertext(0), Ciphertext(1), Ciphertext(2),
                             Ciphertext(3)); -- L0
      R_old := To_Word(Ciphertext(4), Ciphertext(5), Ciphertext(6),
                             Ciphertext(7));  --R0

      -- Round 0..15
      for I in 0..15 loop
         R := L xor F_function(R_Old, Roundkey(I));  -- R1 = L0 + f(R0, K0)
         L := R_Old; -- L1 := R0
         R_Old := R; -- R1
      end loop;


      -- Finale Permutation IP^{-1}
      for I in 0..7 loop
         for J in 0..3 loop
            Ciphertext(I):= Shift_Left(Ciphertext(I),2) xor
              Byte(Shift_Left(Shift_Right(L,IP_RL(I,J)) mod 2 ,1) xor
                   (Shift_Right(R,IP_RL(I,J)) mod 2)) ;
         end loop;
      end loop;

      return Ciphertext;
   end Obsolete_Des;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Obsolete_Prepare_Key(Key        : in  B_Block192;
				  Cipherkey  : out Obsolete_Cipherkey_TDES) is
   begin
      Obsolete_Build_Encrypt_Key(B_Block64(Key( 0.. 7)), 
				 Cipherkey(0).Encrypt_Key);
      Obsolete_Build_Encrypt_Key(B_Block64(Key( 8..15)), 
				 Cipherkey(1).Encrypt_Key);
      Obsolete_Build_Encrypt_Key(B_Block64(Key(16..23)), 
				 Cipherkey(2).Encrypt_Key);

      Obsolete_Build_Decrypt_Key(Cipherkey(0).Encrypt_Key, Cipherkey(0).Decrypt_Key);
      Obsolete_Build_Decrypt_Key(Cipherkey(1).Encrypt_Key, Cipherkey(1).Decrypt_Key);
      Obsolete_Build_Decrypt_Key(Cipherkey(2).Encrypt_Key, Cipherkey(2).Decrypt_Key);

   end Obsolete_Prepare_Key;


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Obsolete_Encrypt(Cipherkey  : in  Obsolete_Cipherkey_TDES;
                               Plaintext  : in  B_Block64;
                               Ciphertext : out B_Block64) is

   begin
      --  3DES EDE-encryption

      Ciphertext := Obsolete_Des
        (Cipherkey(2).Encrypt_Key,
         Obsolete_Des(Cipherkey(1).Decrypt_Key,
                       Obsolete_Des(Cipherkey(0).Encrypt_Key, Plaintext)));
   end Obsolete_Encrypt;


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Obsolete_Decrypt(Cipherkey : in  Obsolete_Cipherkey_TDES;
                              Ciphertext : in  B_Block64;
                              Plaintext  : out B_Block64) is
   begin
      -- 3DES DED-decryption

      Plaintext := Obsolete_Des
        (Cipherkey(0).Decrypt_Key,
         Obsolete_Des(Cipherkey(1).Encrypt_Key,
                       Obsolete_Des(Cipherkey(2).Decrypt_Key, Ciphertext)));

   end Obsolete_Decrypt;


   ---------------------------------------------------------------------------
   -----------------------------NEW CODE--------------------------------------
   ---------------------------------------------------------------------------

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
      -- this Voodoo code is from  Bruce-Schneirs Book
      -- "Applied Cryptography". You only see a few shifts
      -- and S-Box lookups and key additions.
      -- If you dont't trust this code use my obsolete code.
      -- It's only obsolete because it's very slow

      for Round in 0..7 loop
         W  := Shift_Left(Right, 28) or Shift_Right(Right, 4);

         -- We need no expanmsion only a key addition

         W := W xor  Roundkey(J);
         J:=J+1;

         -- SBox transformation (right half):
         -- Here we only xor our result with a few sbox lookups
         -- These s-Box ere not the sbox from the DES spec.
         -- These sboxs contains much more information
         -- and a lot of Voodoo. I fond no paper about
         -- his sboxes. If you have such a paper then mail it,
         -- please.

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

