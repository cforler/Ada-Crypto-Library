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

with Crypto.Types.Random;
with Crypto.Symmetric.Hashfunction_SHA1;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Crypto.Asymmetric.RSA is
   package SHA1 renames Crypto.Symmetric.Hashfunction_SHA1;
   use Big.Utils;
   use Big.Mod_Utils;

-- BIO is new Ada.Text_Io.Modular_IO (Byte);
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
      if Bit_Length(X.N) < Size-1  or Is_Even(X.E) or X.E < 65536 then
	 return False;
      else
	 return True;
      end if;
   end Check_Public_Key; pragma Inline(Check_Public_Key);

   ---------------------------------------------------------------------------

   function Check_Private_Key(X : Private_Key_RSA) return Boolean is
   begin
      if Bit_Length(X.N) < Size-1 or Is_Even(X.D) or (Bit_Length(X.Phi) < Size-2)
        or X.P*X.Q /= X.N or (X.P-1)*(X.Q-1) /= X.Phi or Gcd(X.D, X.P) /= 1
	or Gcd(X.D, X.Q) /= Big_Unsigned_One then
	 return False;
      else
	 return True;
      end if;
   end Check_Private_Key; pragma Inline(Check_Private_Key);

   ---------------------------------------------------------------------------

   procedure Gen_Key (Public_Key               :    out Public_Key_RSA;
                      Private_Key              :    out Private_Key_RSA;
		      Small_Default_Exponent_E : in     Boolean := True)
   is
      P, Q, N, Phi, E, D : Big_Unsigned;

      --  Phi must be set before Find_E can be called.
      function Find_E return Big_Unsigned
      is
	 Small_Primes : constant Words :=
	   (65537, 65539,  65543, 65557, 65609, 65617 );

	 Possible_E : Big_Unsigned;
      begin
	 if Small_Default_Exponent_E then
	    for I in Small_Primes'Range loop
	       Possible_E :=  To_Big_Unsigned (Small_Primes (I));

	       if Gcd (Possible_E, Phi) = Big_Unsigned_One then
		  return Possible_E;
	       end if;
	    end loop;
	 end if;

	 loop
	    Possible_E := Get_Random (Phi);

	    if Gcd (Possible_E, Phi) = Big_Unsigned_One and Possible_E > 65536 then
	       return Possible_E;
	    end if;
	 end loop;
      end Find_E;

   begin
      P := Get_N_Bit_Prime (Size / 2);
      loop
         Q := Get_N_Bit_Prime (Size / 2);
         exit when P /= Q;
      end loop;

      -- For generation of the Chinese Remainder Theorem (CRT) forms of the
      -- private exponent, we reqiure P > Q.  We already know P /= Q, so...
      if Q > P then
	 Swap (P, Q);
      end if;

      N   := P * Q;
      Phi := (P - 1) * (Q - 1);
      E   := Find_E;
      D   := Inverse (E, Phi);

      Public_Key := Public_Key_RSA'(N => N,
                                    E => E);

      Private_Key := Private_Key_RSA'(N   => N,
				      D   => D,
				      P   => P,
				      Q   => Q,
				      Phi => Phi);
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
	 raise  Invalid_Private_Key_Error;
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
         raise Invalid_Private_Key_Error;
      end if;
      P := Pow(To_Big_Unsigned(Ciphertext),Private_Key.D,Private_Key.N);
      Plaintext := To_RSA_Number(P);
   end Decrypt;


   ---------------------------------------------------------------------------

   function Verify_Key_Pair(Private_Key : Private_Key_RSA;
                            Public_Key  : Public_Key_RSA) return Boolean is
   begin
      if
	Check_Private_Key(Private_Key) = False or
	Check_Public_Key(Public_Key) = False
	or Public_Key.N /= Private_Key.N
	or Mult(Public_Key.E, Private_Key.D, Private_Key.Phi)  /= Big_Unsigned_One
      then
	 return False;
      else
	 return True;
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
                             P   : out RSA_Number;
                             Q   : out RSA_Number;
                             Phi : out RSA_Number) is
   begin
      N := To_Bytes(Private_Key.N);
      D := (others => 0);
      D(D'Last - (Length_In_Bytes(Private_Key.D) - 1)..D'Last)
        := To_Bytes(Private_Key.D);
      P := (others => 0);
      P(P'Last - (Length_In_Bytes(Private_Key.P) - 1)..P'Last)
        := To_Bytes(Private_Key.P);
      Q := (others => 0);
      Q(Q'Last - (Length_In_Bytes(Private_Key.Q) - 1)..Q'Last)
        := To_Bytes(Private_Key.Q);
      Phi := To_Bytes(Private_Key.Phi);
   end Get_Private_Key;

   ---------------------------------------------------------------------------

   procedure Set_Public_Key(N : in Big_Unsigned;
                            E : in Big_Unsigned;
                            Public_Key : out Public_Key_RSA) is
   begin
      Public_Key.N := N;
      Public_Key.E := E;
      if not(Check_Public_Key(Public_Key)) then
         raise  Constraint_Error;
      end if;
   end Set_Public_Key;

    ---------------------------------------------------------------------------

   procedure Set_Public_Key(N : in RSA_Number;
                            E : in RSA_Number;
                            Public_Key : out Public_Key_RSA) is
   begin
     Set_Public_Key( To_Big_Unsigned(N), To_Big_Unsigned(E), Public_key);
   end Set_Public_Key;

   ---------------------------------------------------------------------------


   procedure Set_Private_Key(N   : in RSA_Number;
                             D   : in RSA_Number;
                             P   : in RSA_Number;
                             Q   : in RSA_Number;
                             Phi : in RSA_Number;
                             Private_Key : out Private_Key_RSA) is
   begin
      Private_Key.N := To_Big_Unsigned(N);
      Private_Key.D := To_Big_Unsigned(D);
      Private_Key.P := To_Big_Unsigned(P);
      Private_Key.Q := To_Big_Unsigned(Q);
      Private_Key.Phi := To_Big_Unsigned(Phi);

      if not(Check_Private_Key(Private_Key)) then
         raise  Constraint_Error;
      end if;
   end Set_Private_Key;


   ---------------------------------------------------------------------------

   procedure Set_Private_Key(N   : in Big_Unsigned;
                             D   : in Big_Unsigned;
                             P   : in Big_Unsigned;
                             Q   : in Big_Unsigned;
                             Phi : in Big_Unsigned;
                             Private_Key : out Private_Key_RSA) is
   begin
      Private_Key.N := N;
      Private_Key.D := D;
      Private_Key.P := P;
      Private_Key.Q := Q;
      Private_Key.Phi := Phi;

      if not(Check_Private_Key(Private_Key)) then
	raise  Constraint_Error;
      end if;
   end Set_Private_Key;

   ---------------------------------------------------------------------------
   ------------------------RSAES-PKCS1-v1_5-----------------------------------
   ---------------------------------------------------------------------------

   procedure Non_Zero_Random_Bytes(Item : out Bytes) is
      B : Byte;
   begin
      for O of Item loop
         Random_Loop:
         loop
            Random.Read(B);
            if B /= 0 then
               O := B;
               exit Random_Loop;
            end if;
         end loop Random_Loop;
      end loop;
   end Non_Zero_Random_Bytes;

   ---------------------------------------------------------------------------

   function V1_5_Encrypt(Public_Key : in  Public_Key_RSA;
                         Plaintext  : in  Bytes) return RSA_Number is

      K : constant Natural := Size/8;
      PS_Length : constant Positive := K - Plaintext'Length - 3;
      PS : Bytes(1 .. PS_Length) := (others => 0);
      EM : Bytes(1 .. K) := (others => 0);
      C : Big_Unsigned;
   begin
      -- 1
      if Plaintext'Length > K - 11 then
         raise Plaintext_Too_Long_Error;
      end if;

      -- 2a
      Non_Zero_Random_Bytes(PS);
      -- 2b
      EM(EM'First .. EM'First + 1) := (0, 2);
      EM(EM'First + 2 .. EM'First + 1 + PS_Length) := PS;
      EM(EM'First + 1 + PS_Length + 1) := 0;
      EM(EM'First + 1 + PS_Length + 2 .. EM'Last) := Plaintext;

      declare
         -- 3a
         M : constant Big_Unsigned := To_Big_Unsigned(EM);
      begin
         -- 3b
         Encrypt(Public_Key, M, C);

         -- 3c
         declare
            Ciphertext : RSA_Number := (others => 0);
         begin
            Ciphertext(K - Length_In_Bytes(C) .. K - 1) := To_Bytes(C);
            return Ciphertext;
         end;
      end;
   end V1_5_Encrypt;

   ---------------------------------------------------------------------------

   function V1_5_Decrypt(Private_Key : in  Private_Key_RSA;
                         Ciphertext  : in  RSA_Number) return Bytes is
      K : constant Natural := Size/8;
      M : RSA_Number;
   begin
      -- 1
      if K < 11 then
         raise Decrypt_Error;
      end if;

      -- 2b
      Decrypt(Private_Key, Ciphertext, M);

      declare
         -- 2c
         Decoding_Failed : Boolean := True;
         Separator : Positive := 1;
      begin
         -- 3
         for I in M'First + 2 .. M'Last loop
            if M(I) = 0 and then Decoding_Failed then
               Separator := I;
               Decoding_Failed := False;
            end if;
         end loop;

         if M(M'First) /= 0 or M(M'First + 1) /= 2 or
	   (Separator - (M'First + 2)) < 8  or Decoding_Failed then
            raise Decrypt_Error;
         end if;

         return M(Separator + 1 .. M'Last);
      end;

   end V1_5_Decrypt;

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

      Context : SHA1.Hash_Context;

   begin

      if Seed_Words'Length < M'Length then
         M(Seed_Words'Range) := Seed_Words;
      end if;

      -- 3
      for C in 0..Last_Round loop
         Context.Initialize;

         -- 3a
         if Seed_Words'Length < M'Length then
            -- Hash (MGF_Seed || C)
            M(Seed_Words'Last+1) := Word(C);

            H := Context.Final_Round(W_Block512(M), M_Final_Length);
         else

            if C > Natural(Byte'Last) then
               raise Constraint_Error;
            end if;

            for I in 1..M_Hash_Rounds  loop
               Context.Update( W_Block512(Seed_Words(First+((I-1)*(M'Length+1))..
						   (First+(I*(M'Length+1))-2))));
            end loop;

            M(M'First..(Seed_Words'Last-Seed_Words_Final_Begin)) :=
              Seed_Words(Seed_Words_Final_Begin..Seed_Words'Last);

            M(Seed_Words'Last-Seed_Words_Final_Begin+1) :=
              Shift_Left(Word(C),(MGF_Seed'Length mod 4)*8);

            H := Context.Final_Round(W_Block512(M),M_Final_Length);
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
      raise Constraint_Size_Error with "Only defined for Size > 511";
   end if;

   ---------------------------------------------------------------------------

end Crypto.Asymmetric.RSA;


