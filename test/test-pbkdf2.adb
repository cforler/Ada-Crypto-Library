with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA1;
with Crypto.Symmetric.Mac.Hmac_SHA256;


package body Test.PBKDF2 is
use Crypto.Types;



------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register PBKDF2 Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out PBKDF2_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, PBKDF2_Test_Add_Bytes'Access,"PBKDF2 Initialization");
		Register_Routine(T, PBKDF2_Test_Encryption'Access,"PBKDF2 Encryption / Decryption");
		Register_Routine(T, PBKDF2_Test_Exceptions'Access,"PBKDF2 Exceptions");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name PBKDF2 Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : PBKDF2_Test) return Test_String is
	begin
		return new String'("PBKDF2 Test");
	end Name;



------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------


   procedure PBKDF2_Test_Add_Bytes(T : in out Test_Cases.Test_Case'Class) is

  --    Sixteen_Bytes_Container : Bytes(0..15);
      Twenty_Bytes_Container : Bytes(0..19);
      Twentyfive_Bytes_Container : Bytes(0..24);
      success : Boolean;
      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2(Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA1,
                                                        To_Message_Type => To_W_Block512,
                                                        To_Bytes        => To_Bytes ,
                                                        "xor"           => "xor" );
      Scheme : PBKDF2.PBKDF2_KDF;

   begin

      Ada.Text_IO.Put_Line("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
      success := Scheme.Initialize(Parameter => 1);
      Scheme.Derive(Salt     => "salt",
             Password => "password",
             Key      => Twenty_Bytes_Container,
             DK_Len   => 20);

      Assert(Twenty_Bytes_Container(0..19)=(16#0c#, 16#60#, 16#c8#, 16#0f#, 16#96#, 16#1f#, 16#0e#, 16#71#,
             				    16#f3#, 16#a9#, 16#b5#, 16#24#, 16#af#, 16#60#, 16#12#, 16#06#,
             				    16#2f#, 16#e0#, 16#37#, 16#a6#), "PBKDF2 failed. (Parameter 1)");

      success := Scheme.Initialize(Parameter => 2);

      Scheme.Derive(Salt     => "salt",
             Password => "password",
             Key      => Twenty_Bytes_Container,
             DK_Len   => 20);

      Assert(Twenty_Bytes_Container(0..19)=(16#ea#, 16#6c#, 16#01#, 16#4d#, 16#c7#, 16#2d#, 16#6f#, 16#8c#,
             				   16#cd#, 16#1e#, 16#d9#, 16#2a#, 16#ce#, 16#1d#, 16#41#, 16#f0#,
             				   16#d8#, 16#de#, 16#89#, 16#57#), "PBKDF2 failed. (Parameter 2)");


      success := Scheme.Initialize(Parameter => 4096);

      Scheme.Derive(Salt     => "salt",
             Password => "password",
             Key      => Twenty_Bytes_Container,
             DK_Len   => 20);

      Assert(Twenty_Bytes_Container(0..19)=(16#4b#, 16#00#, 16#79#, 16#01#, 16#b7#, 16#65#, 16#48#, 16#9a#,
             				    16#be#, 16#ad#, 16#49#, 16#d9#, 16#26#, 16#f7#, 16#21#, 16#d0#,
             				    16#65#, 16#a4#, 16#29#, 16#c1#), "PBKDF2 failed. (Parameter 4096)");

      success := Scheme.Initialize(Parameter => 4096);

      Scheme.Derive(Salt     => "saltSALTsaltSALTsaltSALTsaltSALTsalt",
             Password => "passwordPASSWORDpassword",
             Key      => Twentyfive_Bytes_Container,
             DK_Len   => 25);

      Assert(Twentyfive_Bytes_Container(0..24)=(16#3d#, 16#2e#, 16#ec#, 16#4f#, 16#e4#, 16#1c#, 16#84#, 16#9b#,
             16#80#, 16#c8#, 16#d8#, 16#36#, 16#62#, 16#c0#, 16#e4#, 16#4a#,
             16#8b#, 16#29#, 16#1a#, 16#96#, 16#4c#, 16#f2#, 16#f0#, 16#70#,
             16#38#), "PBKDF2 failed. (Parameter 4096 long Salt + Password)");

   end PBKDF2_Test_Add_Bytes;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure PBKDF2_Test_Encryption(T : in out Test_Cases.Test_Case'Class) is

   begin


      Assert(True, "Fail at PBKDF2 Test");

   end PBKDF2_Test_Encryption;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure PBKDF2_Test_Exceptions(T : in out Test_Cases.Test_Case'Class) is
   begin

   	   Assert(True, "Aussage");

   end PBKDF2_Test_Exceptions;

------------------------------------------------------------------------------------

end Test.PBKDF2;
