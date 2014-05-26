with AUnit.Assertions;
with Crypto.Symmetric.MAC.CMAC;
with Crypto.Symmetric.Oneway_Blockcipher_AES128;
with Crypto.Symmetric.Oneway_Blockcipher_AES192;
with Crypto.Types;
with Crypto.Types.Random;
with Ada.Text_IO;

pragma Elaborate_All(Crypto.Symmetric.Mac.Cmac);
pragma Elaborate_All(Crypto.Types);

package body Test.CMAC is
   use Crypto.Types;
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   package AES128 renames  Crypto.Symmetric.Oneway_Blockcipher_AES128;
   package CMAC128 is new  Crypto.Symmetric.MAC.CMAC(
				C => AES128,
				"xor" => "xor");
   package AES192 renames  Crypto.Symmetric.Oneway_Blockcipher_AES192;
   package CMAC192 is new  Crypto.Symmetric.MAC.CMAC(
				C => AES192,
				"xor" => "xor");

   Key128 : constant B_Block128 := (16#2b#, 16#7e#, 16#15#, 16#16#, 16#28#, 16#ae#,
			   16#d2#, 16#a6#, 16#ab#, 16#f7#, 16#15#, 16#88#,
			   16#09#, 16#cf#, 16#4f#, 16#3c#);

   Tag128: B_Block128;
   Tag128_To_Be_Tested : B_Block128;

   Key192 : constant B_Block192 := (16#8e#, 16#73#, 16#b0#, 16#f7#, 16#da#, 16#0e#,
			   16#64#, 16#52#, 16#c8#, 16#10#, 16#f3#, 16#2b#,
			   16#80#, 16#90#, 16#79#, 16#e5#, 16#62#, 16#f8#, 
			   16#ea#, 16#d2#, 16#52#, 16#2c#, 16#6b#, 16#7b#);

   Tag192: B_Block128;
   Tag192_To_Be_Tested : B_Block128;
  
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ---------------------------- Register CMAC Tests  --------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------

    procedure Register_Tests(T : in out CMAC_Test) is
       use Test_Cases.Registration;
    begin
      Register_Routine(T, CMAC_Test1'Access,"Known Answer CMAC_Test1 with AES128.");
      Register_Routine(T, CMAC_Test2'Access,"Known Answer CMAC_Test2 with AES128.");
      Register_Routine(T, CMAC_Test3'Access,"Known Answer CMAC_Test3 with AES128.");
      Register_Routine(T, CMAC_Test4'Access,"Known Answer CMAC_Test4 with AES128.");
      Register_Routine(T, CMAC_Test5'Access,"Known Answer CMAC_Test5 with AES192.");
      Register_Routine(T, CMAC_Test6'Access,"Known Answer CMAC_Test6 with AES192.");
      Register_Routine(T, CMAC_Test7'Access,"Known Answer CMAC_Test7 with AES192.");
      Register_Routine(T, CMAC_Test8'Access,"Known Answer CMAC_Test8 with AES192.");

    end Register_Tests;

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ---------------------------- Name CMAC Test --------------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------

    function Name(T : CMAC_Test) return Test_String is
    begin
       return new String'("CMAC Test");
    end Name;

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ------------------------------ Start Tests ---------------------------------
    ----------------------------------------------------------------------------
    --------------------------------- Test 1 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test1(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC128;
       use Ada.Text_IO;
      Message: constant CMAC128.Blocks(1..1) := (1=>(others=>0));
    begin
       Tag128_To_Be_Tested:= (16#bb#, 16#1d#, 16#69#, 16#29#, 16#e9#, 16#59#,
			   16#37#, 16#28#, 16#7f#, 16#a3#, 16#7d#, 16#12#,
			   16#9b#, 16#75#, 16#67#, 16#46#);
       Sign(Message, Key128, Tag128);
  
       Assert(Verify(Message, Key128, Tag128_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test1;

    ----------------------------------------------------------------------------
    --------------------------------- Test 2 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test2(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC128;
       use Ada.Text_IO;
      Message: constant CMAC128.Blocks(1..1) := 
		      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#));
    begin
       Tag128_To_Be_Tested:= (16#07#, 16#0a#, 16#16#, 16#b4#, 16#6b#, 16#4d#,
			   16#41#, 16#44#, 16#f7#, 16#9b#, 16#dd#, 16#9d#,
			   16#d0#, 16#4a#, 16#28#, 16#7c#);
       Sign(Message, Key128, Tag128);
  
       Assert(Verify(Message, Key128, Tag128_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test2;

    ----------------------------------------------------------------------------
    --------------------------------- Test 3 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test3(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC128;
       use Ada.Text_IO;
      Message: constant CMAC128.Blocks(1..3) := 
                      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#),
                       2=>(16#ae#, 16#2d#, 16#8a#, 16#57#, 16#1e#, 16#03#,
			   16#ac#, 16#9c#, 16#9e#, 16#b7#, 16#6f#, 16#ac#,
			   16#45#, 16#af#, 16#8e#, 16#51#),
                       3=>(16#30#, 16#c8#, 16#1c#, 16#46#, 16#a3#, 16#5c#,
			   16#e4#, 16#11#, others=> 0));
    begin
       Tag128_To_Be_Tested:= (16#df#, 16#a6#, 16#67#, 16#47#, 16#de#, 16#9a#,
			   16#e6#, 16#30#, 16#30#, 16#ca#, 16#32#, 16#61#,
			   16#14#, 16#97#, 16#c8#, 16#27#);
       Sign(Message, Key128, Tag128);
  
       Assert(Verify(Message, Key128, Tag128_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test3;

    ----------------------------------------------------------------------------
    --------------------------------- Test 4 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test4(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC128;
       use Ada.Text_IO;
      Message: constant CMAC128.Blocks(1..4) := 
		      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#),
                       2=>(16#ae#, 16#2d#, 16#8a#, 16#57#, 16#1e#, 16#03#,
			   16#ac#, 16#9c#, 16#9e#, 16#b7#, 16#6f#, 16#ac#,
			   16#45#, 16#af#, 16#8e#, 16#51#),
                       3=>(16#30#, 16#c8#, 16#1c#, 16#46#, 16#a3#, 16#5c#,
			   16#e4#, 16#11#, 16#e5#, 16#fb#, 16#c1#, 16#19#,
			   16#1a#, 16#0a#, 16#52#, 16#ef#),
                       4=>(16#f6#, 16#9f#, 16#24#, 16#45#, 16#df#, 16#4f#,
			   16#9b#, 16#17#, 16#ad#, 16#2b#, 16#41#, 16#7b#,
			   16#e6#, 16#6c#, 16#37#, 16#10#));
    begin
       Tag128_To_Be_Tested:= (16#51#, 16#f0#, 16#be#, 16#bf#, 16#7e#, 16#3b#,
			   16#9d#, 16#92#, 16#fc#, 16#49#, 16#74#, 16#17#,
			   16#79#, 16#36#, 16#3c#, 16#fe#);
       Sign(Message, Key128, Tag128);
  
       Assert(Verify(Message, Key128, Tag128_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test4;

    ----------------------------------------------------------------------------
    --------------------------------- Test 5 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test5(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC192;
       use Ada.Text_IO;
      Message: constant CMAC192.Blocks(1..1) := (1=>(others=>0));
    begin
       Tag192_To_Be_Tested:= (16#d1#, 16#7d#, 16#df#, 16#46#, 16#ad#, 16#aa#,
			   16#cd#, 16#e5#, 16#31#, 16#ca#, 16#c4#, 16#83#,
			   16#de#, 16#7a#, 16#93#, 16#67#);
       Sign(Message, Key192, Tag192);
  
       Assert(Verify(Message, Key192, Tag192_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test5;

    ----------------------------------------------------------------------------
    --------------------------------- Test 6 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test6(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC192;
       use Ada.Text_IO;
      Message: constant CMAC192.Blocks(1..1) := 
		      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#));
    begin
       Tag192_To_Be_Tested:= (16#9e#, 16#99#, 16#a7#, 16#bf#, 16#31#, 16#e7#,
			   16#10#, 16#90#, 16#06#, 16#62#, 16#f6#, 16#5e#,
			   16#61#, 16#7c#, 16#51#, 16#84#);
       Sign(Message, Key192, Tag192);
  
       Assert(Verify(Message, Key192, Tag192_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test6;

    ----------------------------------------------------------------------------
    --------------------------------- Test 7 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test7(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC192;
       use Ada.Text_IO;
      Message: constant CMAC192.Blocks(1..3) := 
		      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#),
                       2=>(16#ae#, 16#2d#, 16#8a#, 16#57#, 16#1e#, 16#03#,
			   16#ac#, 16#9c#, 16#9e#, 16#b7#, 16#6f#, 16#ac#,
			   16#45#, 16#af#, 16#8e#, 16#51#),
                       3=>(16#30#, 16#c8#, 16#1c#, 16#46#, 16#a3#, 16#5c#,
			   16#e4#, 16#11#, others=> 0));
    begin
       Tag192_To_Be_Tested:= (16#8a#, 16#1d#, 16#e5#, 16#be#, 16#2e#, 16#b3#,
			   16#1a#, 16#ad#, 16#08#, 16#9a#, 16#82#, 16#e6#,
			   16#ee#, 16#90#, 16#8b#, 16#0e#);
       Sign(Message, Key192, Tag192);
  
       Assert(Verify(Message, Key192, Tag192_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test7;
 
    ----------------------------------------------------------------------------
    --------------------------------- Test 8 -----------------------------------
    ----------------------------------------------------------------------------

    procedure CMAC_Test8(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions;
       use CMAC192;
       use Ada.Text_IO;
      Message: constant CMAC192.Blocks(1..4) := 
		      (1=>(16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#,
			   16#9f#, 16#96#, 16#e9#, 16#3d#, 16#7e#, 16#11#,
			   16#73#, 16#93#, 16#17#, 16#2a#),
                       2=>(16#ae#, 16#2d#, 16#8a#, 16#57#, 16#1e#, 16#03#,
			   16#ac#, 16#9c#, 16#9e#, 16#b7#, 16#6f#, 16#ac#,
			   16#45#, 16#af#, 16#8e#, 16#51#),
                       3=>(16#30#, 16#c8#, 16#1c#, 16#46#, 16#a3#, 16#5c#,
			   16#e4#, 16#11#, 16#e5#, 16#fb#, 16#c1#, 16#19#,
			   16#1a#, 16#0a#, 16#52#, 16#ef#),
                       4=>(16#f6#, 16#9f#, 16#24#, 16#45#, 16#df#, 16#4f#,
			   16#9b#, 16#17#, 16#ad#, 16#2b#, 16#41#, 16#7b#,
			   16#e6#, 16#6c#, 16#37#, 16#10#));
    begin
       Tag192_To_Be_Tested:= (16#a1#, 16#d5#, 16#df#, 16#0e#, 16#ed#, 16#79#,
			   16#0f#, 16#79#, 16#4d#, 16#77#, 16#58#, 16#96#,
			   16#59#, 16#f3#, 16#9a#, 16#11#);
       Sign(Message, Key192, Tag192);
  
       Assert(Verify(Message, Key192, Tag192_To_Be_Tested),"CMAC verifying message failed");
    end CMAC_Test8;
----------------------------------------------------------------------------------

end Test.CMAC;
