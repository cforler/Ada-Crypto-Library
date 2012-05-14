with AUnit.Assertions;
with Crypto.Types;
use Crypto.Types;

package body Test.Types is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register Random Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Types_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Types_Test1'Access,"R to DWord");
      Register_Routine(T, Types_Test2'Access,"R to Bytes");
      Register_Routine(T, Types_Test3'Access,"Bytes xor Byte");
      Register_Routine(T, Types_Test4'Access,"B_Block64 to Bytes");
      Register_Routine(T, Types_Test5'Access,"B_Block128 to Bytes");
      Register_Routine(T, Types_Test6'Access,"B_Block192 to Bytes");
      Register_Routine(T, Types_Test7'Access,"B_Block256 to Bytes");
      Register_Routine(T, Types_Test8'Access,"W_Blocks to Bytes");
      Register_Routine(T, Types_Test9'Access,"Is_Zero");
      Register_Routine(T, Types_Test10'Access,"+ Operator: Words and Word");
      Register_Routine(T, Types_Test11'Access,"+ Operator: Bytes and Byte");
      Register_Routine(T, Types_Test12'Access,"+ Operator: Words and Byte");
      Register_Routine(T, Types_Test13'Access,"+ Operator: DWords and DWord");
      Register_Routine(T, Types_Test14'Access,"+ Operator: DWords and Byte");
      Register_Routine(T, Types_Test15'Access,"Characters to Word");
      Register_Routine(T, Types_Test16'Access,"DWord to Byte_DWord");
      Register_Routine(T, Types_Test17'Access,"Words xor Words");      
      Register_Routine(T, Types_Test21'Access,"DWords to Bytes");
      Register_Routine(T, Types_Test22'Access,"Word to Hex (String)");
      Register_Routine(T, Types_Test23'Access,"DWord to Hex (String)");
      Register_Routine(T, Types_Test24'Access,"Left Part");
      Register_Routine(T, Types_Test25'Access,"Right Part");
      Register_Routine(T, Types_Test26'Access,"DW_Block512 to Bytes");
      Register_Routine(T, Types_Test27'Access,"Bytes to B_Block64");
      Register_Routine(T, Types_Test28'Access,"Bytes to B_Block128");
      Register_Routine(T, Types_Test29'Access,"Bytes to B_Block192");
      Register_Routine(T, Types_Test30'Access,"Bytes to B_Block256");
      Register_Routine(T, Types_Test31'Access,"Words xor Words Constraint_Error Test");
      Register_Routine(T, Types_Test32'Access,"Bytes xor Bytes Constraint_Error Test");
      Register_Routine(T, Types_Test33'Access,"DWords xor DWords Constraint_Error Test");
      Register_Routine(T, Types_Test34'Access,"Byte to Hex");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Random Tests ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Types_Test) return Test_String is
   begin
      return new String'("Types Tests");
   end Name;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.Byte_DWord := (2, 1, 1, 1, 1, 1, 1, 1);
      O : Crypto.Types.DWord;
   begin
      O := Crypto.Types.R_To_DWord(I);

      Assert(O = 2#0000000100000001000000010000000100000001000000010000000100000010#, "R to DWord failed");
   end Types_Test1;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.DWord := 2#0000000100000001000000010000000100000001000000010000000100000010#;
      O : Crypto.Types.Byte_DWord;
   begin
      O := Crypto.Types.R_To_Bytes(I);

      Assert(O = (2, 1, 1, 1, 1, 1, 1, 1), "R to Bytes failed");
   end Types_Test2;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 3 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Byte := 2#00000101#;
      I2 : Crypto.Types.Bytes(0..1) := (2#00000001#,2#00000010#);
      O  : Crypto.Types.Bytes(0..1);
      Confirmed_O : Crypto.Types.Bytes(0..1) := (2#00000001#,2#00000111#);
   begin
      O := I2 xor I1;

      Assert(O = Confirmed_O, "Bytes xor Byte failed");
   end Types_Test3;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 4 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.B_Block64;
      O : Crypto.Types.Bytes(0..7);
      Equal : Boolean := true;
   begin
      I := (1, 2, 3, 4, 5, 6, 7, 8);
      O := Crypto.Types.To_Bytes(I);
      for j in I'Range loop
         if I(j) /= O(j) then
            Equal := false;
         end if;
      end loop;

      Assert(Equal, "B_Block64 to Bytes failed");
   end Types_Test4;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 5 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.B_Block128;
      O : Crypto.Types.Bytes(0..15);
      Equal : Boolean := true;
   begin
      I := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
      O := Crypto.Types.To_Bytes(I);
      for j in I'Range loop
         if I(j) /= O(j) then
            Equal := false;
         end if;
      end loop;

      Assert(Equal, "B_Block128 to Bytes failed");
   end Types_Test5;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 6 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.B_Block192;
      O : Crypto.Types.Bytes(0..23);
      Equal : Boolean := true;
   begin
      I := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24);
      O := Crypto.Types.To_Bytes(I);
      for j in I'Range loop
         if I(j) /= O(j) then
            Equal := false;
         end if;
      end loop;

      Assert(Equal, "B_Block192 to Bytes failed");
   end Types_Test6;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 7 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.B_Block256;
      O : Crypto.Types.Bytes(0..31);
      Equal : Boolean := true;
   begin
      I := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);
      O := Crypto.Types.To_Bytes(I);
      for j in I'Range loop
         if I(j) /= O(j) then
            Equal := false;
         end if;
      end loop;

      Assert(Equal, "B_Block256 to Bytes failed");
   end Types_Test7;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 8 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test8(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.Words(0..1);
      O : Crypto.Types.Bytes(0..7);
      I_W256 : Crypto.Types.W_Block256 := (others => 0);
      O_W256 : Crypto.Types.Bytes(0..31);
      O2_W256 : Crypto.Types.Bytes(0..31);
      I_W512 : Crypto.Types.W_Block512 := (others => 0);
      O_W512 : Crypto.Types.Bytes(0..63);
      O2_W512 : Crypto.Types.Bytes(0..63);
   begin
      --show that To_Bytes(Words) works, if so, also the other procedures should work
      I(0) := 16#01020304#;
      I(1) := 16#05060708#;
      O := Crypto.Types.To_Bytes(I);

      O_W256 := Crypto.Types.To_Bytes(I_W256);
      O2_W256 := Crypto.Types.To_Bytes(Words(I_W256));

      O_W512 := Crypto.Types.To_Bytes(I_W512);
      O2_W512 := Crypto.Types.To_Bytes(Words(I_W512));

      Assert(O(0) = 16#01# and O(1) = 16#02# and O(2) = 16#03# and O(3) = 16#04#
                    and O(4) = 16#05# and O(5) = 16#06# and O(6) = 16#07#
                    and O(7) = 16#08# and O_W256 = O2_W256 and O_W512 = O2_W512,
                    "W_Blocks to Bytes failed");
   end Types_Test8;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 9 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test9(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..9) := (0,0,0,0,0,0,0,0,0,0);
      I2 : Crypto.Types.Bytes(0..9) := (0,0,0,0,1,0,0,0,0,0);
      O1 : Boolean;
      O2 : Boolean;
   begin
      O1 := Crypto.Types.Is_Zero(I1);
      O2 := Crypto.Types.Is_Zero(I2);

      Assert(O1 and (O2 = False), "Is_Zero Test failed");
   end Types_Test9;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 10 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test10(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Words(0..1) := (16#00000000#,16#FFFFFFFF#);
      I2 : Crypto.Types.Word := 16#00000001#;
      I3 : Crypto.Types.Words(0..2) := (16#FFFFFFFF#,16#FFFFFFFF#, 16#FFFFFFFF#);
      O1 : Crypto.Types.Words(0..1);
      O2 : Crypto.Types.Words(0..1);
      O3 : Crypto.Types.Words(0..2);
   begin
      O1 := I1 + I2;
      O2 := I2 + I1;
      O3 := I3 + I2;

      Assert(O1(0) = 16#00000001# and O1(1) = 16#00000000# and O2 = O1 and O3(0) = 16#00000000#
      		     and O3(1) = 16#00000000# and O3(2) = 16#00000000#, "+ Operator: Words and Word Test failed");
   end Types_Test10;


   -----------------------------------------------------------------------------
   ----------------------------------- Test 11 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test11(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..1) := (16#00#,16#FF#);
      I2 : Crypto.Types.Byte := 16#01#;
      I3 : Crypto.Types.Bytes(0..2) := (16#FF#,16#FF#,16#FF#);
      O1 : Crypto.Types.Bytes(0..1);
      O2 : Crypto.Types.Bytes(0..1);
      O3 : Crypto.Types.Bytes(0..2);
   begin
      O1 := I1 + I2;
      O2 := I2 + I1;
      O3 := I3 + I2;

      Assert(O1(0) = 16#01# and O1(1) = 16#00# and O2 = O1 and O3(0) = 16#00# and O3(1) = 16#00#
           	     and O3(2) = 16#00#, "+ Operator: Bytes and Byte Test failed");
   end Types_Test11;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 12 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test12(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Words(0..1) := (16#00000000#,16#FFFFFFFF#);
      I2 : Crypto.Types.Byte := 16#01#;
      I3 : Crypto.Types.Words(0..2) := (16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#);
      O1 : Crypto.Types.Words(0..1);
      O2 : Crypto.Types.Words(0..2);
   begin
      O1 := I1 + I2;
      O2 := I3 + I2;

      Assert(O1(0) = 16#00000001# and O1(1) = 16#00000000# and O2(0) = 16#00000000#
             	     and O2(1) = 16#00000000# and O2(2) = 16#00000000#, "+ Operator: Words and Byte Test failed");
   end Types_Test12;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 13 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test13(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DWords(0..1) := (16#0000000000000000#,16#FFFFFFFFFFFFFFFF#);
      I2 : Crypto.Types.DWord := 16#0000000000000001#;
      I3 : Crypto.Types.DWords(0..2) := (16#FFFFFFFFFFFFFFFF#,16#FFFFFFFFFFFFFFFF#,16#FFFFFFFFFFFFFFFF#);
      O1 : Crypto.Types.DWords(0..1);
      O2 : Crypto.Types.DWords(0..1);
      O3 : Crypto.Types.DWords(0..2);
   begin
      O1 := I1 + I2;
      O2 := I2 + I1;
      O3 := I3 + I2;

      Assert(O1(0) = 16#0000000000000001# and O1(1) = 16#0000000000000000# and O2 = O1 and
             	     O3(0) = 16#0000000000000000# and O3(1) = 16#0000000000000000# and O3(2) = 16#0000000000000000#, "+ Operator: DWords and DWord Test failed");
   end Types_Test13;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 14 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test14(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DWords(0..1) := (16#0000000000000000#,16#FFFFFFFFFFFFFFFF#);
      I2 : Crypto.Types.Byte := 16#01#;
      I3 : Crypto.Types.DWords(0..2) := (16#FFFFFFFFFFFFFFFF#,16#FFFFFFFFFFFFFFFF#,16#FFFFFFFFFFFFFFFF#);
      O1 : Crypto.Types.DWords(0..1);
      O2 : Crypto.Types.DWords(0..2);
   begin
      O1 := I1 + I2;
      O2 := I3 + I2;

      Assert(O1(0) = 16#0000000000000001# and O1(1) = 16#0000000000000000# and
             	     O2(0) = 16#0000000000000000# and O2(1) = 16#0000000000000000# and O2(2) = 16#0000000000000000#, "+ Operator: DWords and Byte Test failed");
   end Types_Test14;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 15 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test15(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      O : Crypto.Types.Word;
   begin
      O := Crypto.Types.To_Word('A','B','C','D');

      Assert(O = 16#41424344#, "Characters to Word failed");
   end Types_Test15;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 16 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test16(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I : Crypto.Types.DWord := 16#0102030405060708#;
      O : Crypto.Types.Byte_DWord;
   begin
      O := Crypto.Types.To_Bytes(I);

      Assert(O(0) = 16#01# and O(1) = 16#02# and O(2) = 16#03# and O(3) = 16#04# and O(4) = 16#05# and O(5) = 16#06# and O(6) = 16#07# and O(7) = 16#08#, "DWord to Byte_DWord failed");
   end Types_Test16;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 17 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test17(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Words(0..1) := (2#00000000_00000000_00000000_00000101#,2#00000000_00000000_00000000_00000101#);
      I2 : Crypto.Types.Words(0..1) := (2#00000000_00000000_00000000_00000001#,2#00000000_00000000_00000000_00000010#);
      O  : Crypto.Types.Words(0..1);
   begin
      O := I2 xor I1;

      Assert(O(0) = 2#00000000_00000000_00000000_00000100# and O(1) = 2#00000000_00000000_00000000_00000111#, "Words xor Words failed");
   end Types_Test17;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 21 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test21(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DWords(0..0) := (0 => 16#10_20_30_40_50_60_70_80#);
      O1 : Crypto.Types.Bytes(0..7);
   begin
      O1 := Crypto.Types.To_Bytes(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#), "DWords to Bytes failed");
   end Types_Test21;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 22 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test22(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Word := 16#10_20_30_40#;
      O1 : Crypto.Types.Hex_Word;
   begin
      O1 := Crypto.Types.To_Hex(I1);

      Assert(O1 = "10203040", "Word to Hex (String) failed");
   end Types_Test22;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 23 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test23(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DWord := 16#10_20_30_40_50_60_70_80#;
      O1 : Crypto.Types.Hex_DWord;
   begin
      O1 := Crypto.Types.To_Hex(I1);

      Assert(O1 = "1020304050607080", "DWord to Hex (String) failed");
   end Types_Test23;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 24 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test24(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..3) := (16#10#,16#20#,16#30#,16#40#);
      O1 : Crypto.Types.Bytes(0..1);
   begin
      O1 := Crypto.Types.Left_Part(I1);

      Assert(O1 = (16#10#,16#20#), "Left Part failed");
   end Types_Test24;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 25 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test25(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..3) := (16#10#,16#20#,16#30#,16#40#);
      O1 : Crypto.Types.Bytes(0..1);
   begin
      O1 := Crypto.Types.Right_Part(I1);

      Assert(O1 = (16#30#,16#40#), "Left Part failed");
   end Types_Test25;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 26 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test26(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DW_Block512 := (16#10_20_30_40_50_60_70_80#, 16#11_21_31_41_51_61_71_81#,
                                        16#12_22_32_42_52_62_72_82#, 16#13_23_33_43_53_63_73_83#,
                                        16#14_24_34_44_54_64_74_84#, 16#15_25_35_45_55_65_75_85#,
                                       	16#16_26_36_46_56_66_76_86#, 16#17_27_37_47_57_67_77_87#);
      O1 : Crypto.Types.Bytes(0..63);
   begin
      O1 := Crypto.Types.To_Bytes(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
        	   16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#,
        	   16#12#,16#22#,16#32#,16#42#,16#52#,16#62#,16#72#,16#82#,
        	   16#13#,16#23#,16#33#,16#43#,16#53#,16#63#,16#73#,16#83#,
        	   16#14#,16#24#,16#34#,16#44#,16#54#,16#64#,16#74#,16#84#,
        	   16#15#,16#25#,16#35#,16#45#,16#55#,16#65#,16#75#,16#85#,
        	   16#16#,16#26#,16#36#,16#46#,16#56#,16#66#,16#76#,16#86#,
       		   16#17#,16#27#,16#37#,16#47#,16#57#,16#67#,16#77#,16#87#), "DW_Block512 to Bytes failed");
   end Types_Test26;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 27 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test27(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..7) := (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#);
      O1 : Crypto.Types.B_Block64;
   begin
      O1 := Crypto.Types.To_B_Block64(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#), "Bytes to B_Block64 failed");
   end Types_Test27;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 28 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test28(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..15) := (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
                                       	16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#);
      O1 : Crypto.Types.B_Block128;
   begin
      O1 := Crypto.Types.To_B_Block128(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,16#11#,
      		   16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#), "Bytes to B_Block128 failed");
   end Types_Test28;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 29 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test29(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..23) := (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
                                        16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#,
                                       	16#12#,16#22#,16#32#,16#42#,16#52#,16#62#,16#72#,16#82#);
      O1 : Crypto.Types.B_Block192;
   begin
      O1 := Crypto.Types.To_B_Block192(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
                   16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#,
                   16#12#,16#22#,16#32#,16#42#,16#52#,16#62#,16#72#,16#82#), "Bytes to B_Block192 failed");
   end Types_Test29;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 30 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test30(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..31) := (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
                                        16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#,
                                        16#12#,16#22#,16#32#,16#42#,16#52#,16#62#,16#72#,16#82#,
                                       	16#13#,16#23#,16#33#,16#43#,16#53#,16#63#,16#73#,16#83#);
      O1 : Crypto.Types.B_Block256;
   begin
      O1 := Crypto.Types.To_B_Block256(I1);

      Assert(O1 = (16#10#,16#20#,16#30#,16#40#,16#50#,16#60#,16#70#,16#80#,
                   16#11#,16#21#,16#31#,16#41#,16#51#,16#61#,16#71#,16#81#,
                   16#12#,16#22#,16#32#,16#42#,16#52#,16#62#,16#72#,16#82#,
                   16#13#,16#23#,16#33#,16#43#,16#53#,16#63#,16#73#,16#83#), "Bytes to B_Block256 failed");
   end Types_Test30;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 31 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test31(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Words(0..1) := (2#00000000_00000000_00000000_00000101#,2#00000000_00000000_00000000_00000101#);
      I2 : Crypto.Types.Words(0..0) := (0 => 2#00000000_00000000_00000000_00000001#);
      O  : Crypto.Types.Words(0..1);
   begin
      O := I2 xor I1;

      Assert(False, "Constraint_Error was expected but did not occur");
   exception
      when Constraint_Error => Assert(True, "");
      when Error : others => Assert(False, "Constraint_Words_Error was expected but another (unexpected) exception occured");
   end Types_Test31;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 32 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test32(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Bytes(0..1) := (2#00000000#,2#00000000#);
      I2 : Crypto.Types.Bytes(0..0) := (0 => 2#00000000#);
      O  : Crypto.Types.Bytes(0..1);
   begin
      O := I2 xor I1;

      Assert(False, "Constraint_Error was expected but did not occur");
   exception
      when Constraint_Error => Assert(True, "");
      when Error : others => Assert(False, "Constraint_Error was expected but another (unexpected) exception occured");
   end Types_Test32;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 33 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test33(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.DWords(0..1) := (16#00_00_00_00_00_00_00_00#,16#00_00_00_00_00_00_00_00#);
      I2 : Crypto.Types.DWords(0..0) := (0 => 16#00_00_00_00_00_00_00_00#);
      O  : Crypto.Types.DWords(0..1);
   begin
      O := I2 xor I1;

      Assert(False, "Constraint_Error was expected but did not occur");
   exception
      when Constraint_Error => Assert(True, "");
      when Error : others => Assert(False, "Constraint_Error was expected but another (unexpected) exception occured");
   end Types_Test33;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 34 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Types_Test34(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      I1 : Crypto.Types.Byte := 16#00#;
      I2 : Crypto.Types.Byte := 16#25#;
      O1 : Crypto.Types.Hex_Byte;
      O2 : Crypto.Types.Hex_Byte;
   begin
      O1 := Crypto.Types.To_Hex(I1);
      O2 := Crypto.Types.To_Hex(I2);

      Assert(O1 = "00" and O2 = "25", "Constraint_Error was expected but did not occur");
   end Types_Test34;

end Test.Types;
