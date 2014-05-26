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


with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA256;

with Ada.Text_IO;



package body Crypto.Symmetric.KDF_Scrypt is

   --Interface function for static 64 Bytes Output, assuming p=8,
   -- r=8 and N=Security_Parameter
   procedure Derive(This	: in out Scrypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes) is
   begin
      scrypt(Password => To_String(ASCII => Password),
             Salt     => To_String(ASCII => Salt),
             r        => This.r,
             N        => This.N,
             p        => This.p,
             dkLen    => This.dkLen,
             Key      => Key);
   end Derive;


   --function for setting security parameter, used here for setting round count
   procedure Initialize(This	: out Scrypt_KDF;
                        Key_Length: in Natural) is
   begin
      This.dkLen := Key_Length;
   end Initialize;


   procedure Initialize (This	: out Scrypt_KDF;
                         r		: in 	Natural;
                         N		: in 	Natural;
                         p		: in	Natural;
                         dkLen	: in	Natural) is
   begin
      This.dkLen := dkLen;
      This.r := r;
      This.N := N;
      This.p := p;
   end Initialize;



   --core scrypt function
   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes) is

      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2
        (Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA256,
         To_Message_Type => To_W_Block512,
         To_Bytes        => To_Bytes,
         "xor"           => "xor");

      Schema : PBKDF2.PBKDF2_KDF;
      B_Bytes : Bytes(0..p*128*r-1);
      B_W_Blocks : W_Block512_Array(0..p*2*r-1);

   begin

      if not IsPowerOfTwo(N) then
         raise N_not_power_of_2_exception;
      end if;

      Schema.Initialize(Key_Length  => p*128*r,
                        Round_Count => 1);
      Schema.Derive(Salt     => Salt,
                    Password => Password,
                    Key      => B_Bytes);

      for I in B_W_Blocks'Range loop
         B_W_Blocks(I) := To_W_Block512(B_Bytes(I*64..I*64+63));
      end loop;

      for I in 0..p-1 loop
         Error_Output.Put_Line("Scrypt loop "& I'Img & " of " & p'Img);
         B_W_Blocks(I*2*r..I*2*r+2*r-1) :=Scrypt_ROMix
           (Input  => B_W_Blocks(I*2*r..I*2*r+2*r-1),
            N      => N);
      end loop;

      for I in B_W_Blocks'Range loop
         B_Bytes(I*64..I*64+63) := To_Bytes(B_W_Blocks(I));
      end loop;

      Schema.Initialize(Key_Length  => dkLen,
                        Round_Count => 1);
      Schema.Derive(Salt     => B_Bytes,
                    Password => To_Bytes(password),
                    Key      => Key);


   end scrypt;


   --Block rearrangement, used by Scrypt_Block_Mix
   function Scrypt_ROMix(Input	: in 	W_Block512_Array;
                         N	: in 	Natural) return W_Block512_Array is

      type W_Block512_2D_Array is array(Integer range 0..N)
        of W_Block512_Array(Input'Range);

      type pointy is access W_Block512_2D_Array;

      V : constant pointy := new W_Block512_2D_Array;
      X : W_Block512_Array := Input;
      T : W_Block512_Array(Input'Range);
      J : Natural;
      J_Bytes : Bytes(0..63);
      J_Byte_1 : Byte;
      J_Byte_2 : Byte;
      J_Byte_3 : Byte;
      J_Byte_4 : Byte;


   begin

      --basic test whether N=2^x
      if not IsPowerOfTwo(N) then
         raise N_not_power_of_2_exception;
      end if;


      Ada.Text_IO.Put_Line("Passed Declaration");
      for I in 0..N-1 loop
         V(I) := X;
         X := Scrypt_Block_Mix(Input => X);
      end loop;

      for I in 0..N-1 loop

         J_Bytes := To_Bytes(X(X'Last));
         J_Byte_1 := J_Bytes(J_Bytes'First);
         J_Byte_2 := J_Bytes(J_Bytes'First +1);
         J_Byte_3 := J_Bytes(J_Bytes'First +2);
         J_Byte_4 := J_Bytes(J_Bytes'First +3);
         J := (Integer(J_Byte_1)+Integer(J_Byte_2)*2**8+Integer(J_Byte_3)*2**16
               +Integer(J_Byte_4)*2**24) mod N;
         T := X xor V(J);
         X := Scrypt_Block_Mix(Input => T);
      end loop;

      return X;


   end Scrypt_ROMix;


   --Function Scrypt_Block_Mix, used by scrypt
   function Scrypt_Block_Mix(Input	: in W_Block512_Array)
                             return W_Block512_Array is
      X     	 : W_Block512 := Input(Input'Last);
      T		 : W_Block512;
      Pre_Output : W_Block512_Array(Input'Range);
      Output	 : W_Block512_Array(Input'Range);

      Counter : Natural := 0;

   begin

      for I in Input'Range loop
         T := X xor Input(I);
         Salsa20_8(Input  => T ,
                   Output => X);
         Pre_Output(I) := X;
      end loop;

      for I in Pre_Output'Range loop
         if I mod 2 = 0 then
            Error_Output.Put_Line(Integer'Image(Pre_Output'Length)
                                  & " " & Integer'Image(I));
            Output(Counter + Output'First) := Pre_Output(I);
            Counter := Counter+1;
         end if;
      end loop;

      for I in Pre_Output'Range loop
         Error_Output.Put_Line(Integer'Image(I));
      end loop;
      for I in Output'Range loop
         Error_Output.Put_Line(Integer'Image(I));
      end loop;

      for I in Pre_Output'Range loop
         if I mod 2 = 1 then
            Error_Output.Put_Line(Integer'Image(Pre_Output'Length)
                                  & " " & Integer'Image(I));
            Output(Counter + Output'First) := Pre_Output(I);
            Counter := Counter+1;
         end if;
      end loop;

      return Output;

   end Scrypt_Block_Mix;


   --Stream Cipher, used by Scrypt_Block_Mix
   procedure Salsa20_8(Input	: in	W_Block512;
                       Output 	: out	W_Block512) is

      X : W_Block512;
      Input_R : W_Block512;
      Output_R : W_Block512;
      Temp_Bytes_A : Bytes(0..3);
   begin

      for I in Input'Range loop
         Temp_Bytes_A := To_Bytes(Input(I));
         Input_R(I) := To_Word(A => Temp_Bytes_A(3),
                               B => Temp_Bytes_A(2),
                               C => Temp_Bytes_A(1),
                               D => Temp_Bytes_A(0));
      end loop;

      for I in 0..15 loop
         X(I) := Input_R(I);
      end loop;

      for I in 0..3 loop
         X( 4) := X( 4) xor Rotate_Left(X( 0)+X(12), 7);
         X( 8) := X( 8) xor Rotate_Left(X( 4)+X( 0), 9);
         X(12) := X(12) xor Rotate_Left(X( 8)+X( 4),13);
         X( 0) := X( 0) xor Rotate_Left(X(12)+X( 8),18);
         X( 9) := X( 9) xor Rotate_Left(X( 5)+X( 1), 7);
         X(13) := X(13) xor Rotate_Left(X( 9)+X( 5), 9);
         X( 1) := X( 1) xor Rotate_Left(X(13)+X( 9),13);
         X( 5) := X( 5) xor Rotate_Left(X( 1)+X(13),18);
         X(14) := X(14) xor Rotate_Left(X(10)+X( 6), 7);
         X( 2) := X( 2) xor Rotate_Left(X(14)+X(10), 9);
         X( 6) := X( 6) xor Rotate_Left(X( 2)+X(14),13);
         X(10) := X(10) xor Rotate_Left(X( 6)+X( 2),18);
         X( 3) := X( 3) xor Rotate_Left(X(15)+X(11), 7);
         X( 7) := X( 7) xor Rotate_Left(X( 3)+X(15), 9);
         X(11) := X(11) xor Rotate_Left(X( 7)+X( 3),13);
         X(15) := X(15) xor Rotate_Left(X(11)+X( 7),18);
         X( 1) := X( 1) xor Rotate_Left(X( 0)+X( 3), 7);
         X( 2) := X( 2) xor Rotate_Left(X( 1)+X( 0), 9);
         X( 3) := X( 3) xor Rotate_Left(X( 2)+X( 1),13);
         X( 0) := X( 0) xor Rotate_Left(X( 3)+X( 2),18);
         X( 6) := X( 6) xor Rotate_Left(X( 5)+X( 4), 7);
         X( 7) := X( 7) xor Rotate_Left(X( 6)+X( 5), 9);
         X( 4) := X( 4) xor Rotate_Left(X( 7)+X( 6),13);
         X( 5) := X( 5) xor Rotate_Left(X( 4)+X( 7),18);
         X(11) := X(11) xor Rotate_Left(X(10)+X( 9), 7);
         X( 8) := X( 8) xor Rotate_Left(X(11)+X(10), 9);
         X( 9) := X( 9) xor Rotate_Left(X( 8)+X(11),13);
         X(10) := X(10) xor Rotate_Left(X( 9)+X( 8),18);
         X(12) := X(12) xor Rotate_Left(X(15)+X(14), 7);
         X(13) := X(13) xor Rotate_Left(X(12)+X(15), 9);
         X(14) := X(14) xor Rotate_Left(X(13)+X(12),13);
         X(15) := X(15) xor Rotate_Left(X(14)+X(13),18);
      end loop;

      for I in 0..15 loop
         Output_R(I) := X(I)+Input_R(I);
      end loop;

      for I in Output_R'Range loop
         Temp_Bytes_A := To_Bytes(Output_R(I));
         Output(I) := To_Word(A => Temp_Bytes_A(3),
                              B => Temp_Bytes_A(2),
                              C => Temp_Bytes_A(1),
                              D => Temp_Bytes_A(0));
      end loop;

   end;


   --XORing function for type W_Block512_Array
   function "xor" (Left : W_Block512_Array;
                   Right: W_Block512_Array) return W_Block512_Array is
      Output : W_Block512_Array(Left'Range);
   begin
      if Left'Length /= Right'Length then
         raise array_size_not_equal_exception;
      end if;

      for I in Output'Range loop
         Output(I) := Left(I) xor Right(I);
      end loop;

      return Output;

   end "xor";

   --power of two test (rudimentary)
   function IsPowerOfTwo(value : Natural) return Boolean is
   begin
      return value mod 2 = 0;
   end IsPowerOfTwo;





end Crypto.Symmetric.KDF_Scrypt;
