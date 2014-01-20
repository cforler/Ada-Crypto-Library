with Crypto.Types;
use Crypto.Types;

with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA256;

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with System; use System;



package body Crypto.Symmetric.KDF_Scrypt is


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
         X( 4) := X( 4) xor R(X( 0)+X(12), 7);  X( 8) := X( 8) xor R(X( 4)+X( 0), 9);
         X(12) := X(12) xor R(X( 8)+X( 4),13);  X( 0) := X( 0) xor R(X(12)+X( 8),18);
         X( 9) := X( 9) xor R(X( 5)+X( 1), 7);  X(13) := X(13) xor R(X( 9)+X( 5), 9);
         X( 1) := X( 1) xor R(X(13)+X( 9),13);  X( 5) := X( 5) xor R(X( 1)+X(13),18);
         X(14) := X(14) xor R(X(10)+X( 6), 7);  X( 2) := X( 2) xor R(X(14)+X(10), 9);
         X( 6) := X( 6) xor R(X( 2)+X(14),13);  X(10) := X(10) xor R(X( 6)+X( 2),18);
         X( 3) := X( 3) xor R(X(15)+X(11), 7);  X( 7) := X( 7) xor R(X( 3)+X(15), 9);
         X(11) := X(11) xor R(X( 7)+X( 3),13);  X(15) := X(15) xor R(X(11)+X( 7),18);
         X( 1) := X( 1) xor R(X( 0)+X( 3), 7);  X( 2) := X( 2) xor R(X( 1)+X( 0), 9);
         X( 3) := X( 3) xor R(X( 2)+X( 1),13);  X( 0) := X( 0) xor R(X( 3)+X( 2),18);
         X( 6) := X( 6) xor R(X( 5)+X( 4), 7);  X( 7) := X( 7) xor R(X( 6)+X( 5), 9);
         X( 4) := X( 4) xor R(X( 7)+X( 6),13);  X( 5) := X( 5) xor R(X( 4)+X( 7),18);
         X(11) := X(11) xor R(X(10)+X( 9), 7);  X( 8) := X( 8) xor R(X(11)+X(10), 9);
         X( 9) := X( 9) xor R(X( 8)+X(11),13);  X(10) := X(10) xor R(X( 9)+X( 8),18);
         X(12) := X(12) xor R(X(15)+X(14), 7);  X(13) := X(13) xor R(X(12)+X(15), 9);
         X(14) := X(14) xor R(X(13)+X(12),13);  X(15) := X(15) xor R(X(14)+X(13),18);
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

  function Scrypt_Block_Mix(Input	: in W_Block512_Array) return W_Block512_Array is
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
            --Ada.Text_IO.Put_Line(Integer'Image(Pre_Output'Length) & " " & Integer'Image(I));
            Output(Counter + Output'First) := Pre_Output(I);
            Counter := Counter+1;
         end if;
      end loop;
-------------debug

--        for I in Pre_Output'Range loop
--              Ada.Text_IO.Put_Line(Integer'Image(I));
--        end loop;
--        for I in Output'Range loop
--              Ada.Text_IO.Put_Line(Integer'Image(I));
--        end loop;
----------------------/debug

      for I in Pre_Output'Range loop
         if I mod 2 = 1 then
            --Ada.Text_IO.Put_Line(Integer'Image(Pre_Output'Length) & " " & Integer'Image(I));
            Output(Counter + Output'First) := Pre_Output(I);
            Counter := Counter+1;
         end if;
      end loop;

      return Output;

   end Scrypt_Block_Mix;

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





   function Scrypt_ROMix(Input	: in 	W_Block512_Array;
                          N	: in 	Natural) return W_Block512_Array is

      type W_Block512_2D_Array is array(Integer range 0..N) of W_Block512_Array(Input'Range);

      V : W_Block512_2D_Array;
      X : W_Block512_Array := Input;
      T : W_Block512_Array(Input'Range);
      J : Natural;
      J_Bytes : Bytes(0..63);
      J_Byte_1 : Byte;
      J_Byte_2 : Byte;
      J_Byte_3 : Byte;
      J_Byte_4 : Byte;

      J_Word : Word;

   begin

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
         J := (Integer(J_Byte_1)+Integer(J_Byte_2)*2**8+Integer(J_Byte_3)*2**16+Integer(J_Byte_4)*2**24) mod N;


         T := X xor V(J);
         X := Scrypt_Block_Mix(Input => T);
      end loop;

      return X;


   end Scrypt_ROMix;

   function Reverse_Bits(Input : Byte) return Byte is
      Byte0 : Byte :=(2#10000000#);
      Byte1 : Byte :=(2#01000000#);
      Byte2 : Byte :=(2#00100000#);
      Byte3 : Byte :=(2#00010000#);
      Byte4 : Byte :=(2#00001000#);
      Byte5 : Byte :=(2#00000100#);
      Byte6 : Byte :=(2#00000010#);
      Byte7 : Byte :=(2#00000001#);
      Return_Byte : Byte :=(2#00000000#);
   begin

--        Ada.Integer_Text_IO.Put(Integer(Input));
--        ada.Text_IO.New_Line;
--        Ada.Integer_Text_IO.Put(Item  => Integer(Input),
--                                Width => 8,
--                                Base  => 2);
--        Ada.Text_IO.New_Line;

      Return_Byte := Return_Byte or Shift_Right(Byte0 and Input, 7);
      Return_Byte := Return_Byte or Shift_Right(Byte1 and Input, 5);
      Return_Byte := Return_Byte or Shift_Right(Byte2 and Input, 3);
      Return_Byte := Return_Byte or Shift_Right(Byte3 and Input, 1);
      Return_Byte := Return_Byte or Shift_Left(Byte4 and Input, 1);
      Return_Byte := Return_Byte or Shift_Left(Byte5 and Input, 3);
      Return_Byte := Return_Byte or Shift_Left(Byte6 and Input, 5);
      Return_Byte := Return_Byte or Shift_Left(Byte7 and Input, 7);

--        Ada.Integer_Text_IO.Put(Item  => Integer(Return_Byte),
--                                Width => 8,
--                                Base  => 2);
--        ada.Text_IO.New_Line;
--        Ada.Integer_Text_IO.Put(Integer(Return_Byte));
--        ada.Text_IO.New_Line;

      return Return_Byte;
   end;



   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     r		: in 	Natural;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes) is

      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2(Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA256,
                                                        To_Message_Type => To_W_Block512,
                                                        To_Bytes        => To_Bytes,
                                                        "xor"           => "xor");



      Schema : PBKDF2.PBKDF2_KDF;
      B_Bytes : Bytes(0..p*128*r-1);
      B_W_Blocks : W_Block512_Array(0..p*2*r-1);

      Success : Boolean;

   begin
      Success := Schema.Initialize(Parameter => 1);
      Schema.Derive(Salt     => Salt,
                    Password => Password,
                    Key      => B_Bytes,
                    DK_Len   => p*128*r);

      for I in B_W_Blocks'Range loop
         B_W_Blocks(I) := To_W_Block512(B_Bytes(I*64..I*64+63));
      end loop;

      for I in 0..p-1 loop
         Ada.Text_IO.Put_Line("Scrypt loop "& I'Img & " of " & p'Img);
         B_W_Blocks(I*2*r..I*2*r+2*r-1) :=Scrypt_ROMix(Input  => B_W_Blocks(I*2*r..I*2*r+2*r-1),
                                                       N      => N);
      end loop;

      for I in B_W_Blocks'Range loop
         B_Bytes(I*64..I*64+63) := To_Bytes(B_W_Blocks(I));
      end loop;

      Schema.Derive(Salt     => B_Bytes,
                    Password => To_Bytes(password),
                    Key      => Key,
                    DK_Len   => dkLen);


   end scrypt;




   function R(A : Word;
              B : Natural) return Word is
   begin
      return Rotate_Left(A, B);
   end;

end Crypto.Symmetric.KDF_Scrypt;
