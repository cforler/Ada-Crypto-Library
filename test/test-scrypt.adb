with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Crypto.Symmetric.KDF_Scrypt;
use Crypto.Symmetric.KDF_Scrypt;
with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA256;


package body Test.Scrypt is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


   package Scrypt renames Crypto.Symmetric.KDF_Scrypt;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register PBKDF2 Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out Scrypt_Test) is
		use Test_Cases.Registration;
	begin
      		Register_Routine(T, Scrypt_Test_Salsa'Access,"Salsa 20/8 for Scrypt");
      		Register_Routine(T, Scrypt_Test_Block_Mix'Access,"Block Mix for Scrypt");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name PBKDF2 Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Scrypt_Test) return Test_String is
	begin
		return new String'("Scrypt Test");
	end Name;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------


   procedure Scrypt_Test_Salsa(T : in out Test_Cases.Test_Case'Class) is
      Salsa_Bytes : Bytes(0..63) :=
        (16#7e#, 16#87#, 16#9a#, 16#21#, 16#4f#, 16#3e#, 16#c9#, 16#86#,
         16#7c#, 16#a9#, 16#40#, 16#e6#, 16#41#, 16#71#, 16#8f#, 16#26#,
         16#ba#, 16#ee#, 16#55#, 16#5b#, 16#8c#, 16#61#, 16#c1#, 16#b5#,
         16#0d#, 16#f8#, 16#46#, 16#11#, 16#6d#, 16#cd#, 16#3b#, 16#1d#,
         16#ee#, 16#24#, 16#f3#, 16#19#, 16#df#, 16#9b#, 16#3d#, 16#85#,
         16#14#, 16#12#, 16#1e#, 16#4b#, 16#5a#, 16#c5#, 16#aa#, 16#32#,
         16#76#, 16#02#, 16#1d#, 16#29#, 16#09#, 16#c7#, 16#48#, 16#29#,
         16#ed#, 16#eb#, 16#c6#, 16#8d#, 16#b8#, 16#b8#, 16#c2#, 16#5e#);

      Salsa_Output : W_Block512;
      Salsa_Bytes_Out : Bytes(0..63);

   begin
      Ada.Text_IO.Put_Line("sha512crypt salsa:");

      Crypto.Symmetric.KDF_Scrypt.Salsa20_8(Input  => To_W_Block512(Salsa_Bytes),
                                            Output => Salsa_Output);

      Salsa_Bytes_Out := To_Bytes(Salsa_Output);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("--------------------------------------------");
      for I in Salsa_Bytes_Out'Range loop
         Ada.Text_IO.Put(To_Hex(Salsa_Bytes_Out(I)));
         if I mod 16 = 15 then
            Ada.Text_IO.New_Line;
         end if;

      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("--------------------------------------------");


      Assert(True, "Success!");

   end Scrypt_Test_Salsa;

   ------------------------------------------------------------------------------------

   procedure Scrypt_Test_Block_Mix(T : in out Test_Cases.Test_Case'Class) is
      Input_Bytes_A : Bytes(0..63) :=
        (16#f7#, 16#ce#, 16#0b#, 16#65#, 16#3d#, 16#2d#, 16#72#, 16#a4#,
         16#10#, 16#8c#, 16#f5#, 16#ab#, 16#e9#, 16#12#, 16#ff#, 16#dd#,
         16#77#, 16#76#, 16#16#, 16#db#, 16#bb#, 16#27#, 16#a7#, 16#0e#,
         16#82#, 16#04#, 16#f3#, 16#ae#, 16#2d#, 16#0f#, 16#6f#, 16#ad#,
         16#89#, 16#f6#, 16#8f#, 16#48#, 16#11#, 16#d1#, 16#e8#, 16#7b#,
         16#cc#, 16#3b#, 16#d7#, 16#40#, 16#0a#, 16#9f#, 16#fd#, 16#29#,
         16#09#, 16#4f#, 16#01#, 16#84#, 16#63#, 16#95#, 16#74#, 16#f3#,
         16#9a#, 16#e5#, 16#a1#, 16#31#, 16#52#, 16#17#, 16#bc#, 16#d7#);

      Input_Bytes_B : Bytes(0..63) :=
        (16#89#, 16#49#, 16#91#, 16#44#, 16#72#, 16#13#, 16#bb#, 16#22#,
         16#6c#, 16#25#, 16#b5#, 16#4d#, 16#a8#, 16#63#, 16#70#, 16#fb#,
         16#cd#, 16#98#, 16#43#, 16#80#, 16#37#, 16#46#, 16#66#, 16#bb#,
         16#8f#, 16#fc#, 16#b5#, 16#bf#, 16#40#, 16#c2#, 16#54#, 16#b0#,
         16#67#, 16#d2#, 16#7c#, 16#51#, 16#ce#, 16#4a#, 16#d5#, 16#fe#,
         16#d8#, 16#29#, 16#c9#, 16#0b#, 16#50#, 16#5a#, 16#57#, 16#1b#,
         16#7f#, 16#4d#, 16#1c#, 16#ad#, 16#6a#, 16#52#, 16#3c#, 16#da#,
         16#77#, 16#0e#, 16#67#, 16#bc#, 16#ea#, 16#af#, 16#7e#, 16#89#);

      Input : W_Block512_Array(0..1) := (To_W_Block512(Input_Bytes_A), To_W_Block512(Input_Bytes_B));
      Real_Output : W_Block512_Array(0..1);

      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2(Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA256,
                                                	To_Message_Type => To_W_Block512,
                                                	To_Bytes        => Crypto.Types.To_Bytes,
                                                        "xor"           => Crypto.Types."xor");

      PBKDF2_Scheme : PBKDF2.PBKDF2_KDF;
      success : Boolean;
      Pbkdf2_Bytes : Bytes(0..63);

      Key_Bytes : Bytes(0..63);


   begin


      Ada.Text_IO.Put_Line("sha512crypt block mix:");

      Real_Output := Scrypt.Scrypt_Block_Mix(Input  => Input);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("--------------------------------------------");
      for I in Real_Output(0)'Range loop
         Ada.Text_IO.Put(To_Hex(Real_Output(0)(I)));
         if I mod 16 = 15 then
            Ada.Text_IO.New_Line;
         end if;

      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("--------------------------------------------");

      -----------------------------------------------------------------
      Scrypt.Scrypt_ROMix(Input  => Input,
                          N      => 8192,
                          Output => Real_Output);


      Ada.Text_IO.Put_Line("Output ROMix 8192");
      for I in Real_Output(0)'Range loop
         Ada.Text_IO.Put(To_Hex(Real_Output(0)(I)));
         if I mod 16 = 15 then
            Ada.Text_IO.New_Line;
         end if;

      end loop;

      -------------------------------------------------------------------

      success := PBKDF2_Scheme.Initialize(Parameter => 80000);

      PBKDF2_Scheme.Derive(Salt     => "NaCl",
                           Password => "Password",
                           Key      => Pbkdf2_Bytes,
                           DK_Len   => 64);


      ada.Text_IO.Put_Line("PBKDF2 256 Output");
      for I in Pbkdf2_Bytes'Range loop
         Ada.Text_IO.Put(To_Hex(Pbkdf2_Bytes(I)) &" ");
      end loop;


      --------------------------------------------------------------------
      Scrypt.scrypt(Password => "password",
                    Salt     => "NaCl",
                    r        => 4,
                    N        => 8,
                    p        => 16,
                    dkLen    => 64,
                    Key      => Key_Bytes);

      Ada.Text_IO.Put_Line("----------SCRYPT ERGEBNIS-----------------");
      for I in Key_Bytes'Range loop
         Ada.Text_IO.Put(To_Hex(Key_Bytes(I)) &" ");
      end loop;


      Assert(True, "Success!");

   end Scrypt_Test_Block_Mix;

------------------------------------------------------------------------------------

end Test.Scrypt;
