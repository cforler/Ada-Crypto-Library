with Crypto.Types;
use Crypto.Types;

with Ada.Text_IO;
with Ada.Integer_Text_IO;


package body Crypto.Symmetric.KDF_Scrypt is


   procedure Salsa20_8(Input	: in	W_Block512;
                       Output 	: out	W_Block512) is

      X : W_Block512;
   begin

      for I in 0..15 loop
         X(I) := Input(I);
      end loop;


--        Ada.Text_IO.Put_Line("X:");
--        for I in X'Range loop
--           Ada.Text_IO.Put(To_Hex(X(I)));
--           if I mod 4 = 3 then
--              Ada.Text_IO.New_Line;
--           end if;

--        end loop;


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
         Output(I) := X(I) + Input(I);
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
            Output(Counter) := Pre_Output(I);
            Counter := Counter+1;
         end if;
      end loop;

      for I in Pre_Output'Range loop
         if I mod 2 = 1 then
            Output(Counter) := Pre_Output(I);
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





   procedure Scrypt_ROMix(Input	: in 	W_Block512_Array;
                          N	: in 	Natural;
                          Output: out	W_Block512_Array) is

      type W_Block512_2D_Array is array(Integer range 0..N) of W_Block512_Array(Input'Range);

      V : W_Block512_2D_Array;
      X : W_Block512_Array := Input;
      T : W_Block512_Array(Input'Range);
      J : Natural;

   begin

      for I in 0..N-1 loop
         V(I) := X;
         X := Scrypt_Block_Mix(Input => X);
      end loop;

      for I in 0..N-1 loop
         J := Integer(To_Bytes(X(X'Last))(To_Bytes(X(X'Last))'Last)) mod N;
         --Ada.Integer_Text_IO.put(J);

         T := X xor V(J);

         X := Scrypt_Block_Mix(Input => T);

      end loop;

      Output := X;



   end Scrypt_ROMix;


   procedure scrypt (Password 	: in 	String;
                     Salt 	: in 	String;
                     N		: in 	Natural;
                     p		: in	Natural;
                     dkLen	: in	Natural;
                     Key	: out 	Bytes) is
   begin

   end scrypt;




   function R(A : Word;
              B : Natural) return Word is
   begin
      return Rotate_Left(A, B);
   end;

end Crypto.Symmetric.KDF_Scrypt;
