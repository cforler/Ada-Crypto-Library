with AUnit.Assertions; 
with Ada.Text_IO; use Ada.Text_IO;
with Crypto.Symmetric.AEAD_SIV;
with Crypto.Types;
with Interfaces;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Crypto.Symmetric.Blockcipher_AES128;
with Ada.Containers.Indefinite_Vectors;
with Crypto.Symmetric.Mac.CMAC;

package body Test.SIV is
   use Crypto.Types;
   
   --------------------------------------------------------------------------
   ------------------------------- Type - Declaration ------------------------
   --------------------------------------------------------------------------
   
   function Inc(Item: B_Block128) return B_Block128 is
      Result: B_Block128 := (16#09#,16#f9#,16#11#,16#02#,16#9d#,16#74#,
                             16#e3#,16#5b#,16#d8#,16#41#,16#56#,16#c5#,
                             16#63#,16#56#,16#88#,16#c0#);
   begin
      return Result;
   end Inc;

	
   package Nonces is new Crypto.Types.Nonces(B_Block128);
   package Nonce_CTR is new Nonces.Nonces_Ctr(Inc => Inc);
   N: Nonce_CTR.Nonce_Ctr;
   Nonce_IV: constant B_Block128 := (others => 0);

   package SIV is new Crypto.Symmetric.AEAD_SIV
     (BC            => Crypto.Symmetric.Blockcipher_AES128,
      N             => Nonces,
      To_Block_Type => To_B_Block128,
      To_Bytes      => To_Bytes,
      Shift_Left    => Shift_Left);
   
   SIV_Type : SIV.AEAD_SIV;

   package Vectors is new 
     Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
				       Element_Type => Bytes);

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
   procedure Put(B: in Bytes) is
   begin
      for I in B'Range loop
         Put(Get_Hex(B(I)));
      end loop;
      New_Line;
   end Put;
   -- Key = K1 || K2
   K1: constant B_Block128 := (16#Ff#,16#Fe#,16#Fd#,16#Fc#,16#Fb#,
                               16#Fa#,16#F9#,16#F8#,16#F7#,16#F6#,
                               16#F5#,16#F4#,16#F3#,16#F2#,16#F1#,16#F0#);
   K2: constant B_Block128 := (16#F0#,16#F1#,16#F2#,16#F3#,16#F4#,
                               16#F5#,16#F6#,16#F7#,16#F8#,16#F9#,
                               16#Fa#,16#Fb#,16#Fc#,16#Fd#,16#Fe#,16#Ff#);
   -- Plaintext. Bytes read : 14
   Message: constant B_Block128 := (16#11#,16#22#,16#33#,16#44#,16#55#,
				    16#66#,16#77#,16#88#,16#99#,16#Aa#,
				    16#Bb#,16#Cc#,16#Dd#,16#Ee#, others => 0);
   -- AD = Header1 || Header2
   Header1: constant B_Block128 := (16#10#,16#11#,16#12#,16#13#,16#14#,
				    16#15#,16#16#,16#17#,16#18#,16#19#,
				    16#1a#,16#1b#,16#1c#,16#1d#,16#1e#,
				    16#1f#);
   Header2: constant B_Block128 := (16#20#,16#21#,16#22#,16#23#,16#24#,
				    16#25#,16#26#,16#27#, others => 0); -- bytes read: 8

   Header_Index: Natural := 0;
   Header_Array: constant array(0..1) of B_Block128 := (Header1, Header2);
   
   C_Vector, C_Vector2: Vectors.Vector;
   Plain_Array: Vectors.Vector;
   -- Desired answer
   Desired_IV : constant B_Block128 := (16#85#,16#63#,16#2d#,16#07#,16#c6#,
                                16#e8#,16#f3#,16#7f#,16#95#,16#0a#,16#cd#,
                                16#32#,16#0a#,16#2e#,16#cc#,16#93#);
   Desired_C : constant Bytes := (16#40#,16#c0#,16#2b#,16#96#,16#90#,16#c4#,16#dc#,
                          16#04#,16#da#,16#ef#,16#7f#,16#6a#,16#fe#,16#5c#);
   
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
   begin
      B := To_Bytes(Message);
      Count := 14;
   end Read_Plaintext;

   ---------- Read_Header ----------
   procedure Read_Header (B : out Bytes; Count: out Natural) is
   begin
      if Header_Index = 0 then
         Count := 16;
      else
         Count := 8;
      end if;
      if Header_Index < 2 then
         B := To_Bytes(Header_Array(Header_Index));
         Header_Index := Header_Index + 1;
      end if;
   end Read_Header;

   WC: constant SIV.AE.Callback_Writer := Write_Ciphertext'Access;
   RP: constant SIV.AE.Callback_Reader := Read_Plaintext'Access;
   RH: constant SIV.AE.Callback_Reader := Read_Header'Access;

   ---------- Write_Plaintext ----------
   procedure Write_Plaintext (B : in Bytes) is
   begin
      Plain_Array.Append(B);
   end Write_Plaintext;

   -----------Write_Desired_Output-------

   procedure Write_Desired_Output (B : in Bytes) is
   begin
      C_Vector2.Append(B);
   end Write_Desired_Output;

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

   WP: constant SIV.AE.Callback_Writer := Write_Plaintext'Access;
   RC: constant SIV.AE.Callback_Reader := Read_Ciphertext'Access;
   RCA: constant SIV.AE.Callback_Reader := Read_Ciphertext_Again'Access;


   ------------------------ Register SIV Test 1 --------------------------------
	
   procedure Register_Tests(T : in out SIV_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, SIV_Test1'Access,"SIV Known Answer Test1.");
   end Register_Tests;
   
   
   ----------------------------- Name SIV Test -------------------------------
   
   function Name(T : SIV_Test) return Test_String is
   begin
      return new String'("AEAD SIV Test");
   end Name;

   -------------------------------- Start Tests -------------------------------

   procedure SIV_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      --Encrypt
      Nonce_CTR.Initialize(This      => N,
                           File_Path => "nonce.txt",
                           IV        => Nonce_IV);
      
      SIV.Init_Encrypt(This        => SIV_Type,
                       Key1        => K1,
                       Key2        => K2); 

      SIV.Encrypt(This             => SIV_Type,
                  Read_Header      => RH,
                  Read_Plaintext   => RP,
                  Write_Ciphertext => WC);
      Nonce_CTR.Finalize(This => N);

      -- Decrypt
     
      Write_Desired_Output(To_Bytes(Desired_IV));
      Write_Desired_Output(Desired_C);
     
      Header_Index := 0;
      Nonce_CTR.Initialize(This      => N,
                           File_Path => "nonce.txt",
                           IV        => Nonce_IV);
      SIV.Init_Decrypt(This        => SIV_Type,
                       Key1        => K1,
                       Key2        => K2);

      Assert(SIV.Decrypt_And_Verify(This            => SIV_Type,
                                    Read_Header           => RH,
                                    Read_Ciphertext       => RC,
                                    Read_Ciphertext_Again => RCA,
                                    Write_Plaintext       => WP),
	     "AEAD SIV verifying message failed");
      Nonce_CTR.Finalize(This => N);
  
   end SIV_Test1;

end Test.SIV;
