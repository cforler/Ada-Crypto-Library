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

with Crypto.Symmetric.Algorithm.Twofish.Tables;
use Crypto.Symmetric.Algorithm.Twofish.Tables;

--with  Ada.Text_IO; use Ada.Text_IO;
--with  Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Crypto.Symmetric.Algorithm.Twofish is

--   package WIO is new Ada.Text_Io.Modular_IO (Word);
--   package BIO is new Ada.Text_Io.Modular_IO (Byte);

   S : array(0..3,Byte'First..Byte'Last) of Word;   -- pre-computed S-boxes

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function H02(I : Byte; L : Bytes) return Word is
   begin
      return MDS(0, Q0(Q0(I) xor L(L'First+8)) xor L(L'First));
   end H02; pragma Inline (H02);

   ---------------------------------------------------------------------------

   function H12(I : Byte; L : Bytes) return Word is
   begin
      return   MDS(1, Q0(Q1(I) xor L(L'First+9)) xor L(L'First+1));
   end H12; pragma Inline (H12);

   ---------------------------------------------------------------------------

   function H22(I : Byte; L : Bytes) return Word is
   begin
      return MDS(2, q1(Q0(I) xor L(L'First+10)) xor L(L'First+2));
   end H22; pragma Inline (H22);

   ---------------------------------------------------------------------------

   function H32(I : Byte; L : Bytes) return Word is
   begin
      return   MDS(3, q1(q1(I) xor L(L'First+11)) xor L(L'First+3));
   end H32; pragma Inline (H32);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function H03(I : Byte; L : Bytes) return Word is
   begin
      return  H02( q1(I) xor L(L'First+16), L );
   end H03; pragma Inline (H03);

   ---------------------------------------------------------------------------

   function H13(I : Byte; L : Bytes) return Word is
   begin
      return  H12( q1(I) xor L(L'First+17), L );
   end H13; pragma Inline (H13);

   ---------------------------------------------------------------------------

   function H23(I : Byte; L : Bytes) return Word is
   begin
      return H22( q0(I) xor L(L'First+18), L );
   end H23; pragma Inline (H23);

   ---------------------------------------------------------------------------

   function H33(I : Byte; L : Bytes) return Word is
   begin
      return  H32( q0(I) xor L(L'First+19), L );
   end H33; pragma Inline (H33);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   function H04(I : Byte; L : Bytes) return Word is
   begin
      return  H03( q1(I) xor L(L'First+24), L );
   end H04; pragma Inline (H04);

   ---------------------------------------------------------------------------

   function H14(I : Byte; L : Bytes) return Word is
   begin
      return  H13( q0(I) xor L(L'First+25), L );
   end H14; pragma Inline (H14);

   ---------------------------------------------------------------------------

   function H24(I : Byte; L : Bytes) return Word is
   begin
      return  H23( q0(I) xor L(L'First+26), L );
   end H24; pragma Inline (H24);

   ---------------------------------------------------------------------------

   function H34(I : Byte; L : Bytes) return Word is
   begin
      return  H33( q1(I) xor L(L'First+27), L );
   end H34; pragma Inline (H34);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   -- Now we can define the h() function given an array of key bytes.
   -- This function is only used in the key schedule, and not to pre-compute
   -- the keyed S-boxes.
   --
   -- In the key schedule, the input is always of the form
   -- k*(1+2**8+2**16+2**24)
   -- so we only provide k as an argument.
   --
   -- Arguments:
   -- k        input to the h() function.
   -- L        pointer to array of key bytes at
   --          offsets 0,1,2,3, ... 8,9,10,11, (16,17,18,19, (24,25,26,27))
   -- kCycles  # key cycles, 2, 3, or 4.

   function H(K : in Byte;
              L : in Bytes;
              KCycles : in Key_Cycles_Type) return Word is
      W: Word;
   begin
      case( KCycles ) is
         -- We code all 3 cases separately for speed reasons.

         when 2 => W:= H02(K, L) xor H12(K, L) xor H22(K, L) xor H32(K, L);

         when 3 => W:= H03(K, L) xor H13(K, L) xor H23(K, L) xor H33(K, L);

         when 4 => W:= H04(K, L) xor H14(K, L) xor H24(K, L) xor H34(K, L);

         when others  =>  raise CONSTRAINED_ERROR;
      end case;

      return W;
   end H; Pragma Inline(H);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   -- Pre-compute the keyed S-boxes.
   -- Fill the pre-computed S-box array in the expanded key structure.
   -- Each pre-computed S-box maps 8 bits to 32 bits.

   -- The S argument contains half the number of bytes of the full key, but is
   -- derived from the full key. (See Twofish specifications for details.)
   -- S has the weird byte input order used by the Hxx macros.
   --
   -- This function takes most of the time of a key expansion.
   --
   -- Arguments:
   -- L        pointer to array of 8*kCycles Bytes containing the S vector.
   -- kCycles  number of key words, must be in the set {2,3,4}
   -- xkey     pointer to Twofish_key structure that will contain the S-boxes.


   procedure Fill_Keyed_Sboxes( L : in Bytes; KCycles : in Key_Cycles_Type) is
   begin
      case ( kCycles ) is
         -- We code all 3 cases separately for speed reasons.

         when 2 =>
            for I in Byte'First..Byte'Last loop
               S(0, I) := H02(I, L);
               S(1, I) := H12(I, L);
               S(2, I) := H22(I, L);
               S(3, I) := H32(I, L);
            end loop;

         when 3 =>
            for I in Byte'First..Byte'Last loop
               S(0, I) := H03(I, L);
               S(1, I) := H13(I, L);
               S(2, I) := H23(I, L);
               S(3, I) := H33(I, L);
            end loop;

         when 4 =>
            for I in Byte'First..Byte'Last loop
               S(0, I) := H04( i, L);
               S(1, I) := H14( i, L);
               S(2, I) := H24( i, L);
               S(3, I) := H34( i, L);
            end loop;

         when others  =>  raise CONSTRAINED_ERROR;

      end case;

   end Fill_Keyed_Sboxes;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------


   procedure Prepare_Key(Key : in Bytes;
                         Roundkey : out Roundkey_Twofish)  is
      Kcycles : constant Key_Cycles_Type := Shift_Right(Byte(Key'Last+7),3);
      I : Byte:=0;
      A, B : Word;
      C, Bx, Bxx  :  Byte;
      Kptr : Natural := Natural(8*KCycles);
      Sptr : Natural := 32;
      T : Natural;

      -- We use a single array to store all key material in,
      -- to simplify the wiping of the key material at the end.
      -- The first 32 bytes contain the actual (padded) cipher key.
      -- The next 32 bytes contain the S-vector in its weird format,
      -- and we have 4 bytes of overrun necessary for the RS-reduction.
      K : Bytes(0..(32+32+4)):=(others=>0);

   begin

      -- Initialisation of K
      K(0..Key'Last):= Key;

      -- We first compute the 40 expanded key words,
      -- formulas straight from the Twofish specifications.

      while I < 40 loop
         -- Due to the byte spacing expected by the h() function
         -- we can pick the bytes directly from the key K.
         -- As we use bytes, we never have the little/big endian
         -- problem.
         --
         -- Note that we apply the rotation function only to simple
         -- variables, as the rotation macro might evaluate its argument
         -- more than once.

         A := H(I,   K,            KCycles);
         B := H(I+1, K(4..K'Last), KCycles);

         B:= Rotate_Left(B,8);

         -- Compute and store the round keys.
         A := A+B;
         B := B+A;
         Roundkey(Integer(I)) := A;
         Roundkey(Integer(I+1)) := Rotate_Left( B, 9 );

         I := I+2;
      end loop;

      -- Wipe variables that contained key material.
      A := A xor A;
      B := B xor B;


      -- And now the dreaded RS multiplication that few seem to understand.
      -- The RS matrix is not random, and is specially designed to compute the
      -- RS matrix multiplication in a simple way.

      -- We work in the field GF(2)(x)/x**8+x**6+x**3+x**2+1.
      -- Note that this is a different field than used for the MDS matrix.
      -- (At least, it is a different representation because all GF(2**8)
      -- representations are equivalent in some form.)
      --
      -- We take 8 consecutive bytes of the key and interpret them as
      -- a polynomial k_0 + k_1 y + k_2 y**2 + ... + k_7 y**7 where
      -- the k_i bytes are the key bytes and are elements of the finite field.
      -- We multiply this polynomial by y**4 and reduce it modulo
      --     y**4 + (x + 1/x)y**3 + (x)y**2 + (x + 1/x)y + 1.
      -- using straightforward polynomial modulo reduction.
      -- The coefficients of the result are the result of the RS
      -- matrix multiplication. When we wrote the Twofish specification,
      -- the original RS definition used the polynomials,
      -- but that requires much more mathematical knowledge.
      -- We were already using matrix multiplication in a finite field for
      -- the MDS matrix, so I re-wrote the RS operation as a matrix
      -- multiplication to reduce the difficulty of understanding it.
      -- Some implementors have not picked up on this simpler method of
      -- computing the RS operation, even though it is mentioned in the
      -- specifications.

      -- It is possible to perform these computations faster by using 32-bit
      -- word operations, but that is not portable and this is not a speed-
      -- critical area.

      -- We explained the 1/x computation when we did the MDS matrix.

      -- The S vector is stored in K(32..64).
      -- The S vector has to be reversed, so we loop cross-wise.

      -- Note the weird byte spacing of the S-vector, to match the even
      -- or odd key words arrays. See the discussion at the Hxx macros for
      -- details.

      -- Loop over all key material
      while  kptr > 0 loop

         kptr := Kptr - 8;

         -- Initialise the polynimial in sptr(0..12)
         -- The first four coefficients are 0 as we have to multiply by y**4.
         -- The next 8 coefficients are from the key material.

         for J in 0..3 loop
            K(Sptr+J) := 0;
         end loop;

         I:=0;
         for J in 4..11 loop
            K(Sptr+J) := K(Kptr+Natural(I));
            I:=I+1;
         end loop;

         -- The 12 bytes starting at sptr are now the coefficients of
         -- the polynomial we need to reduce.

         -- Loop over the polynomial coefficients from high to low
         T := sptr+11;
         -- Keep looping until polynomial is degree 3;

         while T > sptr+3  loop

            -- Pick up the highest coefficient of the poly.
            C := K(T);

          -- Compute x and (x+1/x) times this coefficient.
          -- See the MDS matrix implementation for a discussion of
          -- multiplication by x and 1/x. We just use different
          -- constants here as we are in a
          -- different finite field representation.

            bx  :=  Rs_Poly(C);
            Bxx :=  Rs_Poly_Div(C);

          -- Subtract suitable multiple of
          -- y**4 + (x + 1/x)y**3 + (x)y**2 + (x + 1/x)y + 1
          -- from the polynomial, except that we don't bother
          -- updating t(0) as it will become zero anyway.

            K(t-1) :=  K(t-1) xor bxx;
            K(t-2) :=  K(t-2) xor bx;
            K(t-3) :=  K(t-3) xor bxx;
            K(t-4) :=  K(t-4) xor C;

            -- Go to the next coefficient.
            T:=T-1;

         end loop;

         -- Go to next S-vector word, obeying the weird spacing rules.
         sptr :=  Sptr+8;
      end loop;

      -- Wipe variables that contained key material.
      pragma Warnings(Off,"useless");
      b   := 0;
      Bx  := 0;
      bxx := 0;
      pragma Warnings(On,"useless");
      
      -- And finally, we can compute the key-dependent S-boxes.

      fill_keyed_sboxes(K(32..Natural(32+8*Kcycles)), kCycles);

    -- Wipe array that contained key material.
    pragma Warnings(Off,"useless");
    K := (others => 0);
    pragma Warnings(On,"useless");
   end Prepare_Key;


   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------


 -- The g() function is the heart of the round function.
 -- We have two versions of the g() function, one without an input
 -- rotation and one with.
 -- The pre-computed S-boxes make this pretty simple.
 --
   function G0(X : in Word) return Word is
   begin
      return
        S(0,Byte3(X)) xor S(1,Byte2(X)) xor S(2,Byte1(X)) xor S(3,Byte0(X));
   end G0; pragma Inline (G0);

   ---------------------------------------------------------------------------

   function G1(X : in Word) return Word is
   begin
      return
        S(0,Byte0(X)) xor S(1,Byte3(X)) xor S(2,Byte2(X)) xor S(3,Byte1(X));
   end G1; pragma Inline (G1);


   ---------------------------------------------------------------------------
   ------------------------------ENCRYPT--------------------------------------
   ---------------------------------------------------------------------------


     -- A single round of Twofish. The A,B,C,D are the four state variables,
     -- T0 and T1 are temporaries, xkey is the expanded key, and r the
     -- round number.

   procedure Encrypt_Rnd( A, B     : in Word;
                          C, D     : in out Word;
                          T0, T1   : out Word;
                          Roundkey : in Roundkey_Twofish; R : in Natural) is
   begin
      T0 := G0(A);
      T1 := G1(B);
      C  := C xor (T0+T1+Roundkey(8+2*(r)));
      C  := Rotate_Right(C,1);
      D  := Rotate_Left(D,1);
      D  := D xor (T0+2*T1+Roundkey(8+2*(r)+1));
   end Encrypt_Rnd; pragma Inline (Encrypt_Rnd);

   ---------------------------------------------------------------------------

   -- Encrypt a single cycle, consisting of two rounds.
   -- This avoids the swapping of the two halves.
   -- Parameter r is now the cycle number.
   procedure Encrypt_Cycle(A, B, C, D : in out Word;
                           T0, T1     : out Word;
                           Roundkey : in Roundkey_Twofish; R : in Natural) is
   begin
      Encrypt_Rnd(A,B,C,D,T0,T1, Roundkey, 2*(R) );
      Encrypt_Rnd(C,D,A,B,T0,T1, Roundkey, 2*(R)+1 );
   end Encrypt_Cycle; pragma Inline (Encrypt_Cycle);

   ---------------------------------------------------------------------------

   procedure Encrypt(Roundkey   : in  Roundkey_Twofish;
                     Plaintext  : in  B_Block128;
                     Ciphertext : out B_Block128) is

      A,B,C,D,T0,T1 : Word;     -- Working variables
      Cipher : Bytes(B_Block128'Range);
   begin
      -- Get the four plaintext words xorred with the key
      A :=  To_Word(Plaintext(3), Plaintext(2), Plaintext(1),
                          Plaintext(0))  xor Roundkey(0);
      B :=  To_Word(Plaintext(7), Plaintext(6), Plaintext(5),
                          Plaintext(4))  xor Roundkey(1);
      C :=  To_Word(Plaintext(11), Plaintext(10), Plaintext(9),
                          Plaintext(8)) xor Roundkey(2);
      D :=  To_Word(Plaintext(15), Plaintext(14), Plaintext(13),
                          Plaintext(12)) xor Roundkey(3);

      -- Do 8 cycles (= 16 rounds)
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 0 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 1 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 2 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 3 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 4 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 5 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 6 );
      Encrypt_Cycle(A,B,C,D,T0,T1,Roundkey, 7 );

      -- Store them with the final swap and the output whitening.
      C := C xor Roundkey(4);
      D := D xor Roundkey(5);
      A := A xor Roundkey(6);
      B := B xor Roundkey(7);

      Cipher( 0.. 3) := R_To_Bytes(C);
      Cipher( 4.. 7) := R_To_Bytes(D);
      Cipher( 8..11) := R_To_Bytes(A);
      Cipher(12..15) := R_To_Bytes(B);
      
      Ciphertext := B_Block128(Cipher);
   end Encrypt;

   ---------------------------------------------------------------------------
   -------------------------------DECRYPT--------------------------------------
   ---------------------------------------------------------------------------

   -- A single round of Twofish for decryption. It differs from
   -- ENCRYTP_RND only because of the 1-bit rotations.

   procedure Decrypt_Rnd( A,B        : in Word;
                          C,D        : in out Word;
                          T0, T1     : out Word;
                          Roundkey   : in Roundkey_Twofish; R : Natural ) is
   begin
      T0 := G0(A);
      T1 := G1(B);
      C  := Rotate_Left(C,1);
      C  := C xor (T0 + T1 + Roundkey(8+2*(R)));
      D  := D xor (T0+2*T1 + Roundkey(8+2*(R)+1));
      D  := Rotate_Right(D,1);
   end Decrypt_Rnd; pragma Inline(Decrypt_Rnd);

   ---------------------------------------------------------------------------

   -- Decrypt a single cycle, consisting of two rounds.
   -- This avoids the swapping of the two halves.
   -- Parameter r is now the cycle number.

   procedure Decrypt_Cycle( A, B, C, D : in out Word;
                            T0, T1     : out Word;
                            Roundkey : in Roundkey_Twofish; R : Natural) is
   begin
      Decrypt_Rnd(A, B, C, D, T0, T1, Roundkey ,2*(r)+1);
      Decrypt_Rnd(C, D, A, B, T0, T1, Roundkey ,2*(r)  );
   end Decrypt_Cycle;

    ---------------------------------------------------------------------------


   procedure Decrypt(Roundkey   : in  Roundkey_Twofish;
                     Ciphertext : in  B_Block128;
                     Plaintext  : out B_Block128) is

      A,B,C,D,T0,T1 : Word;   -- Working variables
      Plain : Bytes(B_Block128'Range);

   begin
      -- Get the four plaintext words xorred with the key
      A :=  To_Word(Ciphertext( 3), Ciphertext( 2), Ciphertext( 1),
                          Ciphertext( 0)) xor Roundkey(4);
      B :=  To_Word(Ciphertext( 7), Ciphertext( 6), Ciphertext( 5),
                          Ciphertext( 4)) xor Roundkey(5);
      C :=  To_Word(Ciphertext(11), Ciphertext(10), Ciphertext( 9),
                          Ciphertext( 8)) xor Roundkey(6);
      D :=  To_Word(Ciphertext(15), Ciphertext(14), Ciphertext(13),
                          Ciphertext(12)) xor Roundkey(7);

      -- Do 8 cycles (= 16 rounds)
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 7);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 6);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 5);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 4);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 3);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 2);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 1);
      Decrypt_Cycle(A, B, C, D, T0, T1, Roundkey, 0);

      -- Store them with the final swap and the output whitening
      C := C xor Roundkey(0);
      D := D xor Roundkey(1);
      A := A xor Roundkey(2);
      B := B xor Roundkey(3);

      Plain( 0.. 3) := R_To_Bytes(C);
      Plain( 4.. 7) := R_To_Bytes(D);
      Plain( 8..11) := R_To_Bytes(A);
      Plain(12..15) := R_To_Bytes(B);
      
      Plaintext := B_Block128(Plain);
   end Decrypt;


   ---------------------------------------------------------------------------
   ----------------------------API--------------------------------------------
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --------------------------PREPARE_KEY--------------------------------------
   ---------------------------------------------------------------------------

   procedure Prepare_Key128(Key       : in B_Block128;
                            Cipherkey : out Cipherkey_Twofish128) is
   begin
      Prepare_Key(Bytes(Key),Cipherkey.Roundkey);
   end Prepare_Key128;

   ---------------------------------------------------------------------------

   procedure Prepare_Key192(Key       : in B_Block192;
                            Cipherkey : out Cipherkey_Twofish192) is
   begin
      Prepare_Key(Bytes(Key),Cipherkey.Roundkey);
   end Prepare_Key192;

   ---------------------------------------------------------------------------

   procedure Prepare_Key256(Key       : in B_Block256;
                            Cipherkey : out Cipherkey_Twofish256) is
   begin
      Prepare_Key(Bytes(Key),Cipherkey.Roundkey);
   end Prepare_Key256;

   ---------------------------------------------------------------------------
   ----------------------------ENCRYPT----------------------------------------
   ---------------------------------------------------------------------------


   procedure Encrypt128(Cipherkey  : in  Cipherkey_Twofish128;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Encrypt(Cipherkey.Roundkey, Plaintext, Ciphertext);
   end Encrypt128;

   ---------------------------------------------------------------------------

   procedure Encrypt192(Cipherkey  : in  Cipherkey_Twofish192;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Encrypt(Cipherkey.Roundkey, Plaintext, Ciphertext);
   end Encrypt192;

   ---------------------------------------------------------------------------

   procedure Encrypt256(Cipherkey  : in  Cipherkey_Twofish256;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128) is
   begin
      Encrypt(Cipherkey.Roundkey, Plaintext, Ciphertext);
   end Encrypt256;


   ---------------------------------------------------------------------------
   ----------------------------DECRYPT----------------------------------------
   ---------------------------------------------------------------------------

   procedure Decrypt128(Cipherkey  : in  Cipherkey_Twofish128;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Decrypt(Cipherkey.Roundkey, Ciphertext, Plaintext);
   end Decrypt128;

   ---------------------------------------------------------------------------

   procedure Decrypt192(Cipherkey  : in  Cipherkey_Twofish192;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Decrypt(Cipherkey.Roundkey, Ciphertext, Plaintext);
   end Decrypt192;

   ---------------------------------------------------------------------------

   procedure Decrypt256(Cipherkey  : in  Cipherkey_Twofish256;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128) is
   begin
      Decrypt(Cipherkey.Roundkey, Ciphertext, Plaintext);
   end Decrypt256;

   ---------------------------------------------------------------------------

end  Crypto.Symmetric.Algorithm.Twofish;

