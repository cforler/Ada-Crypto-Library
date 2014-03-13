with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Crypto.Symmetric.KDF_Scrypt;
use Crypto.Symmetric.KDF_Scrypt;
with Crypto.Symmetric.KDF_PBKDF2;
with Crypto.Symmetric.Mac.Hmac_SHA256;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.Hashfunction_Object_SHA512;


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
--        		Register_Routine(T, Scrypt_Test_Block_Mix'Access,"Block Mix for Scrypt");
--        		Register_Routine(T, Scrypt_Test_ROMix'Access,"Rom Mix for Scrypt");
--        		Register_Routine(T, Scrypt_Test_PBKDF2'Access,"PBKDF2 for Scrypt");
--        		Register_Routine(T, Scrypt_Test_SCRYPT'Access,"SCRYPT for Scrypt");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name PBKDF2 Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   function Name(T : Scrypt_Test) return Test_String is
      HS : Crypto.Symmetric.Hashfunction_Object_SHA512.Scheme;
   begin
      HS.Initialize;

      return new String'("Scrypt Test");

	end Name;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------


   procedure Scrypt_Test_Salsa(T : in out Test_Cases.Test_Case'Class) is

      Salsa_Input_Bytes : Bytes(0..63) :=
        (16#7e#, 16#87#, 16#9a#, 16#21#, 16#4f#, 16#3e#, 16#c9#, 16#86#,
         16#7c#, 16#a9#, 16#40#, 16#e6#, 16#41#, 16#71#, 16#8f#, 16#26#,
         16#ba#, 16#ee#, 16#55#, 16#5b#, 16#8c#, 16#61#, 16#c1#, 16#b5#,
         16#0d#, 16#f8#, 16#46#, 16#11#, 16#6d#, 16#cd#, 16#3b#, 16#1d#,
         16#ee#, 16#24#, 16#f3#, 16#19#, 16#df#, 16#9b#, 16#3d#, 16#85#,
         16#14#, 16#12#, 16#1e#, 16#4b#, 16#5a#, 16#c5#, 16#aa#, 16#32#,
         16#76#, 16#02#, 16#1d#, 16#29#, 16#09#, 16#c7#, 16#48#, 16#29#,
         16#ed#, 16#eb#, 16#c6#, 16#8d#, 16#b8#, 16#b8#, 16#c2#, 16#5e#);

      Salsa_Output_Ideal : Bytes(0..63) :=
        (16#a4#,16#1f#,16#85#,16#9c#,16#66#,16#08#,16#cc#,16#99#,
         16#3b#,16#81#,16#ca#,16#cb#,16#02#,16#0c#,16#ef#,16#05#,
         16#04#,16#4b#,16#21#,16#81#,16#a2#,16#fd#,16#33#,16#7d#,
         16#fd#,16#7b#,16#1c#,16#63#,16#96#,16#68#,16#2f#,16#29#,
         16#b4#,16#39#,16#31#,16#68#,16#e3#,16#c9#,16#e6#,16#bc#,
         16#fe#,16#6b#,16#c5#,16#b7#,16#a0#,16#6d#,16#96#,16#ba#,
         16#e4#,16#24#,16#cc#,16#10#,16#2c#,16#91#,16#74#,16#5c#,
         16#24#,16#ad#,16#67#,16#3d#,16#c7#,16#61#,16#8f#,16#81#);

      Salsa_Output : W_Block512;

   begin

      Crypto.Symmetric.KDF_Scrypt.Salsa20_8(Input  => To_W_Block512(Salsa_Input_Bytes),
                                            Output => Salsa_Output);


     Assert(To_Bytes(Salsa_Output) = Salsa_Output_Ideal, "Success!");

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

      Output_Ideal_A : Bytes(0..63) :=
        (16#a4#,16#1f#,16#85#,16#9c#,16#66#,16#08#,16#cc#,16#99#,
         16#3b#,16#81#,16#ca#,16#cb#,16#02#,16#0c#,16#ef#,16#05#,
         16#04#,16#4b#,16#21#,16#81#,16#a2#,16#fd#,16#33#,16#7d#,
         16#fd#,16#7b#,16#1c#,16#63#,16#96#,16#68#,16#2f#,16#29#,
         16#b4#,16#39#,16#31#,16#68#,16#e3#,16#c9#,16#e6#,16#bc#,
         16#fe#,16#6b#,16#c5#,16#b7#,16#a0#,16#6d#,16#96#,16#ba#,
         16#e4#,16#24#,16#cc#,16#10#,16#2c#,16#91#,16#74#,16#5c#,
         16#24#,16#ad#,16#67#,16#3d#,16#c7#,16#61#,16#8f#,16#81#);

      Output_Ideal_B : Bytes(0..63) :=
        (16#20#,16#ed#,16#c9#,16#75#,16#32#,16#38#,16#81#,16#a8#,
         16#05#,16#40#,16#f6#,16#4c#,16#16#,16#2d#,16#cd#,16#3c#,
         16#21#,16#07#,16#7c#,16#fe#,16#5f#,16#8d#,16#5f#,16#e2#,
         16#b1#,16#a4#,16#16#,16#8f#,16#95#,16#36#,16#78#,16#b7#,
         16#7d#,16#3b#,16#3d#,16#80#,16#3b#,16#60#,16#e4#,16#ab#,
         16#92#,16#09#,16#96#,16#e5#,16#9b#,16#4d#,16#53#,16#b6#,
         16#5d#,16#2a#,16#22#,16#58#,16#77#,16#d5#,16#ed#,16#f5#,
         16#84#,16#2c#,16#b9#,16#f1#,16#4e#,16#ef#,16#e4#,16#25#);


      Output : W_Block512_Array(0..1);
      Input : W_Block512_Array(0..1) := (To_W_Block512(Input_Bytes_A), To_W_Block512(Input_Bytes_B));

   begin


      Error_Output.Put_Line("sha512crypt block mix:");

      Output := Scrypt.Scrypt_Block_Mix(Input  => Input);

      Assert(To_W_Block512(Output_Ideal_A) = Output(0), "First Message Part wrong");
      Assert(To_W_Block512(Output_Ideal_B) = Output(1), "Second Message Part wrong");

      Error_Output.New_Line;
      Error_Output.Put_Line("--------------------------------------------");
      for I in Output(0)'Range loop
         Error_Output.Put(To_Hex(Output(0)(I)));
         if I mod 16 = 15 then
            Error_Output.New_Line;
         end if;
      end loop;

      Error_Output.New_Line;
      Error_Output.Put_Line("--------------------------------------------");

      for I in Output(1)'Range loop
         Error_Output.Put(To_Hex(Output(1)(I)));
         if I mod 16 = 15 then
            Error_Output.New_Line;
         end if;
      end loop;

      Error_Output.New_Line;
      Error_Output.Put_Line("--------------------------------------------");

   end Scrypt_Test_Block_Mix;

   -------------------------------------------------

   procedure Scrypt_Test_ROMix(T : in out Test_Cases.Test_Case'Class) is
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
      Output_Ideal_A : Bytes(0..63) :=
        (16#79#, 16#cc#, 16#c1#, 16#93#, 16#62#, 16#9d#, 16#eb#, 16#ca#,
         16#04#, 16#7f#, 16#0b#, 16#70#, 16#60#, 16#4b#, 16#f6#, 16#b6#,
         16#2c#, 16#e3#, 16#dd#, 16#4a#, 16#96#, 16#26#, 16#e3#, 16#55#,
         16#fa#, 16#fc#, 16#61#, 16#98#, 16#e6#, 16#ea#, 16#2b#, 16#46#,
         16#d5#, 16#84#, 16#13#, 16#67#, 16#3b#, 16#99#, 16#b0#, 16#29#,
         16#d6#, 16#65#, 16#c3#, 16#57#, 16#60#, 16#1f#, 16#b4#, 16#26#,
         16#a0#, 16#b2#, 16#f4#, 16#bb#, 16#a2#, 16#00#, 16#ee#, 16#9f#,
         16#0a#, 16#43#, 16#d1#, 16#9b#, 16#57#, 16#1a#, 16#9c#, 16#71#);
      Output_Ideal_B : Bytes(0..63) :=
        (16#ef#, 16#11#, 16#42#, 16#e6#, 16#5d#, 16#5a#, 16#26#, 16#6f#,
         16#dd#, 16#ca#, 16#83#, 16#2c#, 16#e5#, 16#9f#, 16#aa#, 16#7c#,
         16#ac#, 16#0b#, 16#9c#, 16#f1#, 16#be#, 16#2b#, 16#ff#, 16#ca#,
         16#30#, 16#0d#, 16#01#, 16#ee#, 16#38#, 16#76#, 16#19#, 16#c4#,
         16#ae#, 16#12#, 16#fd#, 16#44#, 16#38#, 16#f2#, 16#03#, 16#a0#,
         16#e4#, 16#e1#, 16#c4#, 16#7e#, 16#c3#, 16#14#, 16#86#, 16#1f#,
         16#4e#, 16#90#, 16#87#, 16#cb#, 16#33#, 16#39#, 16#6a#, 16#68#,
         16#73#, 16#e8#, 16#f9#, 16#d2#, 16#53#, 16#9a#, 16#4b#, 16#8e#);

      Input : W_Block512_Array(0..1) := (To_W_Block512(Input_Bytes_A), To_W_Block512(Input_Bytes_B));
      Output : W_Block512_Array(0..1);

   begin

      Output := Scrypt.Scrypt_ROMix(Input  => Input,
                                    N      => 16);

      Assert(To_W_Block512(Output_Ideal_A) = Output(0), "First ROMix Output false");
      Assert(To_W_Block512(Output_Ideal_B) = Output(1), "Second ROMix Output false");

      Error_Output.Put_Line("Output ROMix");
      for I in Output(0)'Range loop
         Error_Output.Put(To_Hex(Output(0)(I)));
         if I mod 4 = 3 then
            Error_Output.New_Line;
         end if;

      end loop;


   end Scrypt_Test_ROMix;

   -------------------------------------------------

   procedure Scrypt_Test_PBKDF2(T : in out Test_Cases.Test_Case'Class) is


      package PBKDF2 is new Crypto.Symmetric.KDF_PBKDF2(Hmac_Package    => Crypto.Symmetric.Mac.Hmac_SHA256,
                                                	To_Message_Type => To_W_Block512,
                                                	To_Bytes        => Crypto.Types.To_Bytes,
                                                        "xor"           => Crypto.Types."xor");

      Ideal_A : Bytes(0..63) :=
        (16#55#, 16#ac#, 16#04#, 16#6e#, 16#56#, 16#e3#, 16#08#, 16#9f#,
         16#ec#, 16#16#, 16#91#, 16#c2#, 16#25#, 16#44#, 16#b6#, 16#05#,
         16#f9#, 16#41#, 16#85#, 16#21#, 16#6d#, 16#de#, 16#04#, 16#65#,
         16#e6#, 16#8b#, 16#9d#, 16#57#, 16#c2#, 16#0d#, 16#ac#, 16#bc#,
         16#49#, 16#ca#, 16#9c#, 16#cc#, 16#f1#, 16#79#, 16#b6#, 16#45#,
         16#99#, 16#16#, 16#64#, 16#b3#, 16#9d#, 16#77#, 16#ef#, 16#31#,
         16#7c#, 16#71#, 16#b8#, 16#45#, 16#b1#, 16#e3#, 16#0b#, 16#d5#,
         16#09#, 16#11#, 16#20#, 16#41#, 16#d3#, 16#a1#, 16#97#, 16#83#);

      Ideal_B : Bytes(0..63) :=
        (16#4d#, 16#dc#, 16#d8#, 16#f6#, 16#0b#, 16#98#, 16#be#, 16#21#,
         16#83#, 16#0c#, 16#ee#, 16#5e#, 16#f2#, 16#27#, 16#01#, 16#f9#,
         16#64#, 16#1a#, 16#44#, 16#18#, 16#d0#, 16#4c#, 16#04#, 16#14#,
         16#ae#, 16#ff#, 16#08#, 16#87#, 16#6b#, 16#34#, 16#ab#, 16#56#,
         16#a1#, 16#d4#, 16#25#, 16#a1#, 16#22#, 16#58#, 16#33#, 16#54#,
         16#9a#, 16#db#, 16#84#, 16#1b#, 16#51#, 16#c9#, 16#b3#, 16#17#,
         16#6a#, 16#27#, 16#2b#, 16#de#, 16#bb#, 16#a1#, 16#d0#, 16#78#,
         16#47#, 16#8f#, 16#62#, 16#b3#, 16#97#, 16#f3#, 16#3c#, 16#8d#);


      PBKDF2_Scheme : PBKDF2.PBKDF2_KDF;
      success : Boolean;
      Pbkdf2_Bytes : Bytes(0..63);



   begin

      success := PBKDF2_Scheme.Initialize(Parameter => 1);

      PBKDF2_Scheme.Derive(Salt     => "salt",
                           Password => "passwd",
                           Key      => Pbkdf2_Bytes,
                           DK_Len   => 64);
      Assert(Ideal_A = Pbkdf2_Bytes, "Success!");

      success := PBKDF2_Scheme.Initialize(Parameter => 80000);

      PBKDF2_Scheme.Derive(Salt     => "NaCl",
                           Password => "Password",
                           Key      => Pbkdf2_Bytes,
                           DK_Len   => 64);
      Assert(Ideal_B = Pbkdf2_Bytes, "Success!");



   end Scrypt_Test_PBKDF2;


   -------------------------------------------------

   procedure Scrypt_Test_SCRYPT(T : in out Test_Cases.Test_Case'Class) is


      Key_Size : Natural := 64;
      Key_Bytes : Bytes(0..Key_Size-1);

      Ideal_One : Bytes(0..63) :=
        (16#77#, 16#d6#, 16#57#, 16#62#, 16#38#, 16#65#, 16#7b#, 16#20#,
         16#3b#, 16#19#, 16#ca#, 16#42#, 16#c1#, 16#8a#, 16#04#, 16#97#,
         16#f1#, 16#6b#, 16#48#, 16#44#, 16#e3#, 16#07#, 16#4a#, 16#e8#,
         16#df#, 16#df#, 16#fa#, 16#3f#, 16#ed#, 16#e2#, 16#14#, 16#42#,
         16#fc#, 16#d0#, 16#06#, 16#9d#, 16#ed#, 16#09#, 16#48#, 16#f8#,
         16#32#, 16#6a#, 16#75#, 16#3a#, 16#0f#, 16#c8#, 16#1f#, 16#17#,
         16#e8#, 16#d3#, 16#e0#, 16#fb#, 16#2e#, 16#0d#, 16#36#, 16#28#,
         16#cf#, 16#35#, 16#e2#, 16#0c#, 16#38#, 16#d1#, 16#89#, 16#06#);

      Ideal_Two : Bytes(0..63) :=
        (16#fd#, 16#ba#, 16#be#, 16#1c#, 16#9d#, 16#34#, 16#72#, 16#00#,
         16#78#, 16#56#, 16#e7#, 16#19#, 16#0d#, 16#01#, 16#e9#, 16#fe#,
         16#7c#, 16#6a#, 16#d7#, 16#cb#, 16#c8#, 16#23#, 16#78#, 16#30#,
         16#e7#, 16#73#, 16#76#, 16#63#, 16#4b#, 16#37#, 16#31#, 16#62#,
         16#2e#, 16#af#, 16#30#, 16#d9#, 16#2e#, 16#22#, 16#a3#, 16#88#,
         16#6f#, 16#f1#, 16#09#, 16#27#, 16#9d#, 16#98#, 16#30#, 16#da#,
         16#c7#, 16#27#, 16#af#, 16#b9#, 16#4a#, 16#83#, 16#ee#, 16#6d#,
         16#83#, 16#60#, 16#cb#, 16#df#, 16#a2#, 16#cc#, 16#06#, 16#40#);
      Ideal_Three : Bytes(0..63) :=
        (16#70#, 16#23#, 16#bd#, 16#cb#, 16#3a#, 16#fd#, 16#73#, 16#48#,
         16#46#, 16#1c#, 16#06#, 16#cd#, 16#81#, 16#fd#, 16#38#, 16#eb#,
         16#fd#, 16#a8#, 16#fb#, 16#ba#, 16#90#, 16#4f#, 16#8e#, 16#3e#,
         16#a9#, 16#b5#, 16#43#, 16#f6#, 16#54#, 16#5d#, 16#a1#, 16#f2#,
         16#d5#, 16#43#, 16#29#, 16#55#, 16#61#, 16#3f#, 16#0f#, 16#cf#,
         16#62#, 16#d4#, 16#97#, 16#05#, 16#24#, 16#2a#, 16#9a#, 16#f9#,
         16#e6#, 16#1e#, 16#85#, 16#dc#, 16#0d#, 16#65#, 16#1e#, 16#40#,
         16#df#, 16#cf#, 16#01#, 16#7b#, 16#45#, 16#57#, 16#58#, 16#87#);

   begin

      ------------------------------------------------------------------
      Scrypt.scrypt(Password => "",
                    Salt     => "",
                    r        => 1,
                    N        => 16,
                    p        => 1,
                    dkLen    => Key_Size,
                    Key      => Key_Bytes);

      Assert(Key_Bytes = Ideal_One, "Scrypt 1 failed");

      Scrypt.scrypt(Password => "password",
                    Salt     => "NaCl",
                    r        => 8,
                    N        => 1024,
                    p        => 16,
                    dkLen    => Key_Size,
                    Key      => Key_Bytes);

      Assert(Key_Bytes = Ideal_Two, "Scrypt 2 failed");

      Scrypt.scrypt(Password => "pleaseletmein",
                    Salt     => "SodiumChloride",
                    r        => 8,
                    N        => 2**14,
                    p        => 1,
                    dkLen    => Key_Size,
                    Key      => Key_Bytes);


      Assert(Key_Bytes = Ideal_Three, "Scrypt 3 failed");


      end Scrypt_Test_SCRYPT;


      ------------------------------------------------------------------------------------

end Test.Scrypt;
