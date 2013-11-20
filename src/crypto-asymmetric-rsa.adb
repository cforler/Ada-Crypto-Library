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

with Ada.Text_IO;
with Crypto.Types.Random;
with Crypto.Symmetric.Hashfunction_SHA1;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Crypto.Asymmetric.RSA is
--   use Big;
   package SHA1 renames Crypto.Symmetric.Hashfunction_SHA1;
   use Big.Utils;
   use Ada.Text_IO;
   use Big.Mod_Utils;


--   package BIO is new Ada.Text_Io.Modular_IO (Byte);
--   package WIO is new Ada.Text_IO.Modular_IO (Word);
--   use BIO, WIO;


   ---------------------------------------------------------------------------

   function To_RSA_Number(X : Big_Unsigned) return RSA_Number is
      R : RSA_Number := (others=> 0);
      N : constant Integer := Length_In_Bytes(X)-1;
   begin
      if N > 0 then R(R'Last-N..R'Last) := To_Bytes(X);
      end if;
      return R;
   end To_RSA_Number;


   ---------------------------------------------------------------------------

   function Check_Public_Key(X :  Public_Key_RSA) return Boolean is
   begin
      if Bit_Length(X.N) /= Size then return False;
      elsif Is_Odd(X.E)  and (X.E > Big_Unsigned_Two) then return True;
      else return False;
      end if;
   end Check_Public_Key; pragma Inline(Check_Public_Key);

   ---------------------------------------------------------------------------

   function Check_Private_Key(X : Private_Key_RSA) return Boolean is
   begin
      if Bit_Length(X.N) /= Size or X.D <= Big_Unsigned_Two
        or Is_Even(X.D) or Is_Odd(X.Phi) or (Bit_Length(X.Phi) < Size-2)
        or Gcd(X.D, X.Phi) /= 1 then return False;
      else return True;
      end if;
   end Check_Private_Key; pragma Inline(Check_Private_Key);

   ---------------------------------------------------------------------------

   procedure Gen_Key(Public_Key  : out Public_Key_RSA;
                     Private_Key : out Private_Key_RSA) is
   begin

      -- First we generate the Private_Key
      -- To save space we set:
      --   Public.Key.N = P
      --   Public.Key.E = Q
      -- I know this is a little bit confusion. ;-)
      loop
         Public_Key.N := Get_N_Bit_Prime(Size/2+1); -- P
         Public_Key.E  := Get_N_Bit_Prime(Size/2);  -- Q

         Private_Key.N := Public_Key.N * Public_Key.E; -- N = P*Q

         exit when Bit_Length(Private_Key.N) = Size;
      end loop;

      Private_Key.Phi := (Public_Key.N-1)  * (Public_Key.E-1);

      --Algorithm to compute E:
      -- 1. chose a random D   (D < S)
      -- 2. has D no inverses then goto 1
      -- 3. compute the inverses of D
      loop
         -- 1.
         Private_Key.D := Get_Random(Private_Key.Phi);

         --2.
         if  Private_Key.D > Big_Unsigned_Two  then
            if  Gcd(Private_Key.D, Private_Key.Phi) = Big_Unsigned_One then

               --3.
               Public_Key.E :=
                 Inverse(Private_Key.D,Private_Key.Phi); -- E*D = 1 mod S
               exit;
            end if;
         end if;
      end loop;

      Public_Key.N :=  Private_Key.N;

   end Gen_Key;

   ---------------------------------------------------------------------------

   procedure Encrypt(Public_Key  : in  Public_Key_RSA;
                     Plaintext   : in  Big_Unsigned;
                     Ciphertext  : out Big_Unsigned) is
   begin
      if Check_Public_Key(Public_Key) = False then
         raise Invalid_Public_Key_Error;
      end if;
      Ciphertext := Pow(Plaintext, Public_Key.E,  Public_Key.N);
   end Encrypt;

   ---------------------------------------------------------------------------

   procedure Encrypt(Public_Key  : in  Public_Key_RSA;
                     Plaintext   : in  RSA_Number;
                     Ciphertext  : out RSA_Number) is
      C : Big_Unsigned;
   begin
      if Check_Public_Key(Public_Key) = False then
         raise Invalid_Public_Key_Error;
      end if;
      C := Pow(To_Big_Unsigned(Plaintext), Public_Key.E,  Public_Key.N);
      Ciphertext := To_RSA_Number(C);
   end Encrypt;

   ---------------------------------------------------------------------------
	
   -- Added for Signature Decryption for Certificate
   procedure Decrypt(Private_Key : in  Private_Key_RSA;
                     Ciphertext  : in  Big_Unsigned;
                     Plaintext   : out Big_Unsigned) is
   begin
      if Check_Private_Key(Private_Key) = False then
         Put("-error");--raise Decrypt_Error;
      end if;
      Plaintext := Pow(Ciphertext, Private_Key.D, Private_Key.N);
   end Decrypt;


   ---------------------------------------------------------------------------

   procedure Decrypt(Private_Key : in  Private_Key_RSA;
                     Ciphertext  : in  RSA_Number;
                     Plaintext   : out RSA_Number) is
      P : Big_Unsigned;
   begin
      if Check_Private_Key(Private_Key) = False then
         raise Decrypt_Error;
      end if;
      P := Pow(To_Big_Unsigned(Ciphertext),Private_Key.D,Private_Key.N);
      Plaintext := To_RSA_Number(P);
   end Decrypt;


   ---------------------------------------------------------------------------

   function Verify_Key_Pair(Private_Key : Private_Key_RSA;
                            Public_Key  : Public_Key_RSA) return Boolean is
   begin
      if ((Check_Private_Key(Private_Key) and Check_Public_Key(Public_Key)) =
        False) or Public_Key.N /= Private_Key.N or
        Mult(Public_Key.E,Private_Key.D,Private_Key.Phi) /= Big_Unsigned_One
      then  return False;
      else return True;
      end if;
   end Verify_Key_Pair;


   ---------------------------------------------------------------------------

   procedure Get_Public_Key(Public_Key : in Public_Key_RSA;
                            N : out RSA_Number;
                            E : out RSA_Number) is
   begin
      N :=  To_Bytes(Public_Key.N);
      E := (others => 0);
      E(E'Last - (Length_In_Bytes(Public_Key.E) - 1)..E'Last)
        := To_Bytes(Public_Key.E);

   end Get_Public_Key;

   ---------------------------------------------------------------------------

   procedure Get_Private_Key(Private_Key : in Private_Key_RSA;
                             N   : out RSA_Number;
                             D   : out RSA_Number;
                             Phi : out RSA_Number) is
   begin
      N := To_Bytes(Private_Key.N);
      D := (others => 0);
      D(D'Last - (Length_In_Bytes(Private_Key.D) - 1)..D'Last)
        := To_Bytes(Private_Key.D);
      Phi := To_Bytes(Private_Key.Phi);
   end Get_Private_Key;

   ---------------------------------------------------------------------------

   procedure Set_Public_Key(N : in RSA_Number;
                            E : in RSA_Number;
                            Public_Key : out Public_Key_RSA) is
   begin
      Public_Key.N := To_Big_Unsigned(N);
      Public_Key.E := To_Big_Unsigned(E);

      if not(Check_Public_Key(Public_Key)) then
         raise  Constraint_Error;
      end if;
   end Set_Public_Key;

   ---------------------------------------------------------------------------

   procedure Set_Private_Key(N   : in RSA_Number;
                             D   : in RSA_Number;
                             Phi : in RSA_Number;
                             Private_Key : out Private_Key_RSA) is
   begin
      Private_Key.N := To_Big_Unsigned(N);
      Private_Key.D := To_Big_Unsigned(D);
      Private_Key.Phi := To_Big_Unsigned(Phi);

      if not(Check_Private_Key(Private_Key)) then
         raise  Constraint_Error;
      end if;

   end Set_Private_Key;

   ---------------------------------------------------------------------------

   procedure Set_Private_Key(N   : in Big_Unsigned;
                             D   : in Big_Unsigned;
                             Phi : in Big_Unsigned;
                             Private_Key : out Private_Key_RSA) is
   begin
      Private_Key.N := N;
      Private_Key.D := D;
      Private_Key.Phi := Phi;
      
      if not(Check_Private_Key(Private_Key)) then
         --raise  Constraint_Error;
	Put("-error");
      end if;
 
   end Set_Private_Key;

   ---------------------------------------------------------------------------
   -----------------------------OAEP-RSA--------------------------------------
   ---------------------------------------------------------------------------

   -- this funtion based on the MGF1 definition.
   -- you find this definition in the PKCS #1 v2.1 Appendix B.2.1 (page 48)

   function  MGF_SHA1(MGF_Seed : Bytes; Mask_Len : Positive)
                     return Bytes is

      -- Hash Output in Bytes;
      Hlen : constant  := 160/8;

      M : Words(0..15) := (others => 0);
      H : W_Block160;

      Seed_Words : constant  Words  := To_Words(MGF_Seed);

      First : constant Natural := Seed_Words'First;

      --  Message_Length_Of_Final_Round
      --  Max-Length 64 (Byte) = 512-Bit
      --  Message-Length = |MGF_Seed|+|Word| = |MGF_Seed| + 4
      M_Final_Length : constant Natural := (MGF_Seed'Length+4) mod 64;

      -- Normal Hash Rounds
      -- How many (normal) SHA-1 rounds?
      M_Hash_Rounds : constant Natural := (MGF_Seed'Length+4) / 64;


      Last_Round : constant Natural :=
        Natural(Float'Ceiling(Float(Mask_Len)/Float(Hlen)))-1;


      Seed_Words_Final_Begin : constant Integer :=
        (First+(M_Hash_Rounds*(M'Length+1))-1);

      T : Bytes(1..(Last_Round+1)*Hlen);
   begin

      if Seed_Words'Length < M'Length then
         M(Seed_Words'Range) := Seed_Words;
      end if;

      -- 3
      for C in 0..Last_Round loop
         SHA1.Init;

         -- 3a
         if Seed_Words'Length < M'Length then
            -- Hash (MGF_Seed || C)
            M(Seed_Words'Last+1) := Word(C);

            H := SHA1.Final_Round(W_Block512(M), M_Final_Length);
         else

            if C > Natural(Byte'Last) then
               raise Constraint_Error;
            end if;

            for I in 1..M_Hash_Rounds  loop
               SHA1.Update( W_Block512(Seed_Words(First+((I-1)*(M'Length+1))..
						   (First+(I*(M'Length+1))-2))));
            end loop;

            M(M'First..(Seed_Words'Last-Seed_Words_Final_Begin)) :=
              Seed_Words(Seed_Words_Final_Begin..Seed_Words'Last);

            M(Seed_Words'Last-Seed_Words_Final_Begin+1) :=
              Shift_Left(Word(C),(MGF_Seed'Length mod 4)*8);

            H := SHA1.Final_Round(W_Block512(M),M_Final_Length);
        end if;

        --3b
         T(1+(Hlen*C)..Hlen*(C+1)) := To_Bytes(H);
      end loop;

      -- 4.
      return T(1..Mask_Len);
   end MGF_SHA1;

   ---------------------------------------------------------------------------

   -- OAEP_Encrypt based on the RSAES-OAEP-ENCRYPT operation
   -- [PKCS #1 v2.1 Chapter 7.1.1 (page 17-19)]

   -- RSAES-OAEP-Encrypt with MGF_SHA1 as MGF
   -- (pre)condition: L is the empty string

   function OAEP_Encrypt(Public_Key : in  Public_Key_RSA;
                         Plaintext  : in  Bytes) return RSA_Number is

      Hlen : constant := 160/8;
      K : constant Natural := Size/8;

      Seed, Seed_Mask : B_Block160;

      -- DB = lhash || PS
      -- L = empty String =>
      -- lhash = 16#da39a3ee_5e6b4b0d_3255bfef_95601890_afd80709#
      DB : Bytes(1..K-Hlen-1) :=
        (16#da#, 16#39#, 16#a3#, 16#ee#, 16#5e#, 16#6b#, 16#4b#, 16#0d#,
         16#32#, 16#55#, 16#bf#, 16#ef#, 16#95#, 16#60#, 16#18#, 16#90#,
         16#af#, 16#d8#, 16#07#, 16#09#,  others => 0);

      DB_Mask : Bytes(1..K-Hlen-1);

   begin
     -- 1b
     if Plaintext'Length > K - 2*Hlen - 2 then
        raise Plaintext_Too_Long_Error;
     end if;

     -- 2c
     DB(DB'Last-Plaintext'Length) := 1;
     DB(DB'Last-Plaintext'Length+1..DB'Last) := Plaintext;

     -- 2d
     Crypto.Types.Random.Read( Bytes(Seed) );

     --2e
     DB_Mask := MGF_SHA1(Bytes(Seed), K-Hlen-1);

     --2f
     DB_Mask := DB xor DB_Mask;

     --2g
     Seed_Mask := B_Block160(MGF_SHA1( Bytes(DB_Mask), Hlen));

     --2h
     Seed_Mask := B_Block160(Bytes(Seed) xor Bytes(Seed_Mask));

     --2i + 3a
     declare
        EM : Big_Unsigned := To_Big_Unsigned(Bytes(Seed_Mask));
        C  : Big_Unsigned;
     begin

        EM := Shift_Left(EM, DB_Mask'Length*Byte'Size) +
          To_Big_Unsigned(DB_Mask);


        -- 3b
        Encrypt(Public_Key, EM, C);

        --3c.
        declare
           Ciphertext : RSA_Number := (others => 0);

        begin
           Ciphertext(K-Length_In_Bytes(C)..K-1) := To_Bytes(C);
           return Ciphertext;
        end;
     end;
   end OAEP_Encrypt;

   ---------------------------------------------------------------------------

   -- OAEP_Decrypt based on the RSAES-OAEP-DECRYPT operation
   -- [PKCS #1 v2.1 Chapter 7.1.2 (page 20-21)]

   -- RSAES-OAEP-DECRYPT with MGF_SHA1 as MGF
   -- (pre)condition: L is the empty string

   function OAEP_Decrypt(Private_Key : in  Private_Key_RSA;
                         Ciphertext  : in  RSA_Number) return Bytes is
      Hlen : constant := 160/8;
      K : constant Natural := Size/8;

      Seed : B_Block160;
      Mask : Bytes(0..K-1) := (others => 0);
      DB : Bytes(1..K-Hlen-1);

      C : constant Big_Unsigned := To_Big_Unsigned(Ciphertext);
      EM : Big_Unsigned;

      -- L = empty string =>
      Hash : constant Bytes :=
        (16#da#, 16#39#, 16#a3#, 16#ee#, 16#5e#, 16#6b#, 16#4b#, 16#0d#,
         16#32#, 16#55#, 16#bf#, 16#ef#, 16#95#, 16#60#, 16#18#, 16#90#,
         16#af#, 16#d8#, 16#07#, 16#09#);

      J : Natural := Hlen+1;

   begin

      if  C > Private_Key.N then raise Decrypt_Error;
      end if;

      --2b
      Decrypt(Private_Key, C, EM);

      --2c   Mask = 0x00 || Masked_Seed || Masked_DB
      Mask(Mask'Last-(Length_In_Bytes(EM)-1)..Mask'Last)
        := To_Bytes(EM);

      -- Y /= 0
      if Mask(0) /= 0 then
         raise Decrypt_Error;
      end if;

      --3c
      Seed := B_Block160(MGF_SHA1(Mask(Seed'Length+1..Mask'Last),Hlen));

      --3d
      Seed := B_Block160(Bytes(Mask(1..Seed'Length)) xor Bytes(Seed));

      --3e
      DB := MGF_SHA1(Bytes(Seed), K-Hlen-1);

      --3f
      DB :=  Mask(Seed'Length+1..Mask'Last) xor DB;

      --3g
      for I in 1..Hlen loop
         if DB(I) /= Hash(Hash'First+I-1) then
            raise Decrypt_Error;
         end if;
      end loop;

      loop
         if DB(J) = 1 then
            if DB(J-1) /= 0 and DB(J-1) /= Hash(Hash'Last) then
               raise Decrypt_Error;
            else
               J:=J+1;
               exit;
            end if;
         elsif DB(J) /= 0 then raise Decrypt_Error;
         end if;

         J:=J+1;

         if J > DB'Last then  raise Decrypt_Error;
         end if;
      end loop;


      -- 4.
      return DB(J..DB'Last);

   end OAEP_Decrypt;

   ---------------------------------------------------------------------------

begin
   if Size < 512 then
      Put_Line("Only defined for Size > 511");
      raise Constraint_Size_Error;
   end if;

   ---------------------------------------------------------------------------

end Crypto.Asymmetric.RSA;


