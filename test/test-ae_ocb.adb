with AUnit.Assertions; 
with Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Crypto.Types;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Symmetric.AE_OCB3;
with Ada.Containers.Indefinite_Vectors;

pragma Elaborate_All(Crypto.Types.Nonces.Nonces_Ctr,Crypto.Symmetric.AE_OCB3);
  
package body Test.AE_OCB is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
   package CT renames Crypto.Types;
   package AES128 renames Crypto.Symmetric.Blockcipher_AES128;
   
   function Inc(Item: CT.B_Block128) return CT.B_Block128 is
      Result: CT.B_Block128 := (16#00#,16#01#,16#02#,16#03#,16#04#,16#05#,
                                16#06#,16#07#,16#08#,16#09#,16#0A#,16#0B#,
				others=>0);
   begin
      return Result;
   end Inc;
   
   package Nonce is new CT.Nonces(CT.B_Block128);
   package Nonce_CTR is new Nonce.Nonces_Ctr(Inc);

   use CT;
   package OCB is new Crypto.Symmetric.AE_OCB3(BC            => AES128,
					       N             => Nonce,
					       To_Block_Type => To_B_Block128,
					       To_Bytes      => To_Bytes,
					       Shift_Left    => Shift_Left,
					       Shift_Right   => Shift_Right,
					       To_Byte_Word  => To_Bytes);

   package Byte_Vector is new Ada.containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                Element_Type => Bytes);

   C_Vector: Byte_Vector.Vector;
   C_Vector2: Byte_Vector.Vector;
   P_Vector: Byte_Vector.Vector;

   Key: constant B_Block128 := (16#00#,16#01#,16#02#,16#03#,16#04#,16#05#,16#06#,16#07#,16#08#,16#09#,16#0a#,16#0b#,16#0c#,16#0d#,16#0e#,16#0f#);
   
   OCB_Type: OCB.AE_OCB;
   N : Nonce_CTR.Nonce_Ctr;

   Counter: Natural := 0;
   Valid_Tag: Boolean;
   
   Desired_Output: constant Bytes(0 .. 55) := (16#BE#,16#A5#,16#E8#,16#79#,16#8D#,16#BE#,16#71#,16#10#,
					       16#03#,16#1C#,16#14#,16#4D#,16#A0#,16#B2#,16#61#,16#22#,
					       16#CE#,16#AA#,16#B9#,16#B0#,16#5D#,16#F7#,16#71#,16#A6#,
					       16#57#,16#14#,16#9D#,16#53#,16#77#,16#34#,16#63#,16#CB#,
					       16#68#,16#C6#,16#57#,16#78#,16#B0#,16#58#,16#A6#,16#35#,
					       16#06#,16#0C#,16#84#,16#67#,16#F4#,16#AB#,16#AB#,16#5E#,
					       16#8B#,16#3C#,16#20#,16#67#,16#A2#,16#E1#,16#15#,16#DC#);
   ----------------------
   -- useful functions --
   ----------------------

   ---------- Get_Hex ----------
   function Get_Hex(B: in Byte) return String is
      S: String(1..2);
      use Interfaces;
      U8: Unsigned_8;

      function To_Hex(B: in Unsigned_8) return Character is
         U: constant Unsigned_8 := B and 16#0F#;
      begin
         case U is
            when 0..9 =>
               return Character'Val(U + 48);
            when 10..15 =>
               return Character'Val(U + 55); -- upper case letter
            when others =>
               raise Constraint_Error;
         end case;
      end To_Hex;

   begin
      U8 := Shift_Right(Unsigned_8(B), 4);
      S(1) := To_Hex(U8);
      U8 := Unsigned_8(B) and 16#0F#;
      S(2) := To_Hex(U8);
      return S;
   end Get_Hex;

   ---------- Put ----------
   procedure Put(B: in CT.Bytes) is
   begin
      for I in B'Range loop
         Put(Get_Hex(B(I)));
      end loop;
      New_Line;
   end Put;

   ---------------------
   ----- callbacks -----
   ---------------------

   ---------- Write_Ciphertext ----------
   procedure Write_Ciphertext (B : in Bytes) is
   begin
      C_Vector.Append(B);
   end Write_Ciphertext;

   ---------- Read_Plaintext ----------
   procedure Read_Plaintext (B : out Bytes; Count: out Natural) is
      M: constant Bytes(0..47) := (16#00#,16#01#,16#02#,16#03#,16#04#,16#05#,16#06#,16#07#,16#08#,16#09#,16#0a#,16#0b#,16#0c#,16#0d#,16#0e#,16#0f#,
16#10#,16#11#,16#12#,16#13#,16#14#,16#15#,16#16#,16#17#,16#18#,16#19#,16#1a#,16#1b#,16#1c#,16#1d#,16#1e#,16#1f#,
16#20#,16#21#,16#22#,16#23#,16#24#,16#25#,16#26#,16#27#,others=>0);  -- Bytes read 40
   begin
      if Counter = 0 then
         B := M(0 .. 15);
         Count := 16;
         Counter := Counter + 1;
      elsif Counter = 1 then
         B := M(16 .. 31);
         Count := 16;
         Counter := Counter + 1;
      elsif Counter = 2 then
         B := M(32 .. 47);
         Count := 8;
         Counter := Counter + 1;
      else
         Count := 0; 
      end if;

   end Read_Plaintext;
   WC: constant OCB.AE.Callback_Writer := Write_Ciphertext'Access;
   RP: constant OCB.AE.Callback_Reader := Read_Plaintext'Access;

   ---------- Read_Ciphertext ----------
   procedure Read_Ciphertext (B : out Bytes; Count: out Natural) is
      use Ada.Containers;
   begin
      if C_Vector.Length = 0 then
         Count := 0;
      else
         if C_Vector.First_Element'Length < B_Block128'Length then
            declare
               X: constant Bytes := C_Vector.First_Element;
            begin
               B(B'First..X'Length-1) := X;
               Count := X'Length;
            end;
         else
	    B := C_Vector.First_Element;
            Count := 16;
         end if;
         C_Vector.Delete_First;
      end if;
   end Read_Ciphertext;

   ---------- Read_Ciphertext_Again ----------
   procedure Read_Ciphertext_Again (B : out Bytes; Count: out Natural) is
      use Ada.Containers;
   begin
      if C_Vector2.Length = 0 then
         Count := 0;
      else
         if C_Vector2.First_Element'Length < B_Block128'Length then
            declare
               X: constant Bytes := C_Vector2.First_Element;
            begin
               B(B'First..X'Length-1) := X;
               Count := X'Length;
            end;
         else
            B := C_Vector2.First_Element;
            Count := 16;
         end if;
         C_Vector2.Delete_First;
      end if;
   end Read_Ciphertext_Again;

   ---------- Write_Plaintext ----------
   procedure Write_Plaintext (B: in Bytes) is
   begin
      P_Vector.Append(B);
   end Write_Plaintext;

   -----------Write_Desired_Output-------

   procedure Write_Desired_Output (B : in Bytes) is
   begin
      C_Vector2.Append(B);
   end Write_Desired_Output;


   RC  : constant OCB.AE.Callback_Reader := Read_Ciphertext'Access;
   RCA : constant OCB.AE.Callback_Reader := Read_Ciphertext_Again'Access;
   WP  : constant OCB.AE.Callback_Writer := Write_Plaintext'Access;

   Taglength : Natural := 16;
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register OCB Test 1 -----------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out OCB_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, OCB_Test1'Access,"OCB Test1.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------- Name OCB Test --------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : OCB_Test) return Test_String is
	begin
		return new String'("AE OCB Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      -------------
      -- Encrypt --
      -------------
      Counter := 0;
      P_Vector.Clear;
      Nonce_CTR.Initialize(This      => N,
                           File_Path => "nonce.txt");

      OCB.Init_Encrypt(This  => OCB_Type,
                       Key   => Key,
                       N_Init     => N,
                       Bytes_Of_N_Read => 12,
                       Taglen => Taglength);

      OCB.Encrypt(This             => OCB_Type,
                  Read_Plaintext   => RP,
                  Write_Ciphertext => WC);
      Nonce_CTR.Finalize(This => N);

      -------------
      -- Decrypt --
      -------------
      ---- if Read_Ciphertext_Again this assignment must be apply:
      Write_Desired_Output(Desired_Output(0..15));
      Write_Desired_Output(Desired_Output(16..31));
      Write_Desired_Output(Desired_Output(32..47));
      Write_Desired_Output(Desired_Output(48..55));

      Nonce_CTR.Initialize(This      => N,
                           File_Path => "nonce.txt");

      OCB.Init_Decrypt(This  => OCB_Type,
                       Key   => Key,
                       N_Init  => N.Update,
                       Bytes_Of_N_Read => 12,
                       Taglen => Taglength);

      Valid_Tag := OCB.Decrypt_And_Verify(This                   => OCB_Type,
                                          Read_Ciphertext        => RC,
                                          Read_Ciphertext_Again  => RCA,
                                          Write_Plaintext        => WP);
      Assert(Valid_Tag,"AEAD OCB verifying message failed");
      Nonce_CTR.Finalize(This => N);

      end OCB_Test1;
end Test.AE_OCB;
