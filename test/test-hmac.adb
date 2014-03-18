with AUnit.Assertions;
with Crypto.Symmetric.Mac.Hmac_SHA256;
with Crypto.Symmetric.Mac.Hmac_SHA512;
with Crypto.Symmetric.Mac.Hmac_SHA512_Object;
with Crypto.Symmetric.Mac.Hmac_SHA256_Object;
with Crypto.Types;
with Ada.Text_IO;

package body Test.Hmac is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register Symmetric Hmac Test  --------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out Hmac_Test) is
	   use Test_Cases.Registration;
        begin
           Register_Routine(T, Hmac_Test1'Access,"Symmetric Hmac SHA256 Known Answer Test1.");
           Register_Routine(T, Hmac_Test2'Access,"Symmetric Hmac SHA256 Known Answer Test2.");
           Register_Routine(T, Hmac_Test3'Access,"Symmetric Hmac SHA256 Known Answer Test3.");
           Register_Routine(T, Hmac_Test4'Access,"Symmetric Hmac SHA512 Known Answer Test4.");
           Register_Routine(T, Hmac_Test5'Access,"Symmetric Hmac SHA512 Known Answer Test5.");
        end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Symmetric Mac Test -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Hmac_Test) return Test_String is
	begin
		return new String'("Symmetric Hmac Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Hmac_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
--        use Crypto.Symmetric.Mac.Hmac_SHA256;
      use Crypto.Symmetric.Mac.Hmac_SHA256_Object;

      My_Context : HMAC_Context;
      Key_1 : W_Block512 :=
        (16#0b_0b_0b_0b#, 16#0b_0b_0b_0b#, 16#0b_0b_0b_0b#, 16#0b_0b_0b_0b#,
         16#0b_0b_0b_0b#, others => 0); -- 20 Bytes

      Message_1 : W_Block512 :=
         (16#48_69_20_54#, 16#68_65_72_65#, others => 0); --8 Bytes

      Tag_SHA_1 : W_Block256;

      Tag_SHA : W_Block256 :=
        (16#b0_34_4c_61#, 16#d8_db_38_53#, 16#5c_a8_af_ce#, 16#af_0b_f1_2b#,
         16#88_1d_c2_00#, 16#c9_83_3d_a7#, 16#26_e9_37_6c#, 16#2e_32_cf_f7#);

   begin
      My_Context.Init(Key_1);
      My_Context.Final_Sign(Message_1, 8, Tag_SHA_1);

      Assert(Tag_SHA_1 = Tag_SHA,"Symmetric Hmac SHA256 Test failed!");

   end Hmac_Test1;
------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Hmac_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Mac.Hmac_SHA256;

      Key_2 : W_Block512 := (16#4a_65_66_65#, others => 0);

      Message_2 : W_Block512 :=
        (16#77_68_61_74#, 16#20_64_6f_20#, 16#79_61_20_77#, 16#61_6e_74_20#,
         16#66_6f_72_20#, 16#6e_6f_74_68#, 16#69_6e_67_3f#, others => 0); --28 Bytes

      Tag_SHA_2 : W_Block256;

      Tag_SHA : W_Block256 :=
        (16#5b_dc_c1_46#, 16#bf_60_75_4e#, 16#6a_04_24_26#, 16#08_95_75_c7#,
         16#5a_00_3f_08#, 16#9d_27_39_83#, 16#9d_ec_58_b9#, 16#64_ec_38_43#);
   begin
      Init(Key_2);
      Final_Sign(Message_2, 28, Tag_SHA_2);

      Assert(Tag_SHA_2 = Tag_SHA,"Symmetric Hmac SHA256 Test failed!");

   end Hmac_Test2;
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Hmac_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Mac.Hmac_SHA256;

      Key_3 : W_Block512 :=
        (16#aa_aa_aa_aa#, 16#aa_aa_aa_aa#, 16#aa_aa_aa_aa#, 16#aa_aa_aa_aa#,
         16#aa_aa_aa_aa#, others => 0); -- 20 Bytes

      Message_3 : W_Block512 :=
        (16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#,
         16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#,
         16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#, 16#dd_dd_dd_dd#,
         16#dd_dd_00_00#, others => 0); -- 50 Bytes

      Tag_SHA_3 : W_Block256;

      Tag_SHA : W_Block256 :=
        (16#77_3e_a9_1e#, 16#36_80_0e_46#, 16#85_4d_b8_eb#, 16#d0_91_81_a7#,
         16#29_59_09_8b#, 16#3e_f8_c1_22#, 16#d9_63_55_14#, 16#ce_d5_65_fe#);
   begin
      Init(Key_3);
      Final_Sign(Message_3, 50, Tag_SHA_3);

      Assert(Tag_SHA_3 = Tag_SHA,"Symmetric Hmac SHA256 Test failed!");

   end Hmac_Test3;
------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Hmac_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      --use Crypto.Symmetric.Mac.Hmac_SHA512;
      use Crypto.Symmetric.Mac.Hmac_SHA512_Object;

      My_Context : HMAC_Context;

      Key_4 : DW_Block1024 :=
        (16#01_02_03_04_05_06_07_08#, 16#09_0a_0b_0c_0d_0e_0f_10#,
         16#11_12_13_14_15_16_17_18#, 16#19_00_00_00_00_00_00_00#,
         others => 0); -- 25 Bytes

      Message_4 : DW_Block1024 :=
        (16#cd_cd_cd_cd_cd_cd_cd_cd#, 16#cd_cd_cd_cd_cd_cd_cd_cd#,
         16#cd_cd_cd_cd_cd_cd_cd_cd#, 16#cd_cd_cd_cd_cd_cd_cd_cd#,
         16#cd_cd_cd_cd_cd_cd_cd_cd#, 16#cd_cd_cd_cd_cd_cd_cd_cd#,
         16#cd_cd_00_00_00_00_00_00#, others => 0); -- 50 Bytes

      Tag_SHA_4 : DW_Block512;

      Tag_SHA : DW_Block512 :=
        (16#b0_ba_46_56_37_45_8c_69#, 16#90_e5_a8_c5_f6_1d_4a_f7#,
         16#e5_76_d9_7f_f9_4b_87_2d#, 16#e7_6f_80_50_36_1e_e3_db#,
         16#a9_1c_a5_c1_1a_a2_5e_b4#, 16#d6_79_27_5c_c5_78_80_63#,
         16#a5_f1_97_41_12_0c_4f_2d#, 16#e2_ad_eb_eb_10_a2_98_dd#);
   begin
      My_Context.Init(Key => Key_4);
      My_Context.Final_Sign(Message_4, 50, Tag_SHA_4);
      My_Context.Final_Sign(Message_4, 50, Tag_SHA_4);

      Assert(Tag_SHA_4 = Tag_SHA,"Symmetric Hmac SHA512 Test failed!");

   end Hmac_Test4;
------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Hmac_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Mac.Hmac_SHA512;
      Key_5 : DW_Block1024 :=
        (16#0c_0c_0c_0c_0c_0c_0c_0c#, 16#0c_0c_0c_0c_0c_0c_0c_0c#,
         16#0c_0c_0c_0c_00_00_00_00#, others => 0); -- 20 Bytes

      Message_5 : DW_Block1024 :=
        (16#54_65_73_74_20_57_69_74#, 16#68_20_54_72_75_6e_63_61#,
         16#74_69_6f_6e_00_00_00_00#, others => 0); --20 Bytes

      Tag_SHA_5 : DW_Block512;

      Tag_SHA : Bytes(1..16) := (16#41#, 16#5f#, 16#ad#, 16#62#, 16#71#, 16#58#,
                                 16#0a#, 16#53#, 16#1d#, 16#41#, 16#79#, 16#bc#,
                                 16#89#, 16#1d#, 16#87#, 16#a6#); -- Standard Tag
      Trunc : Bytes(1..16);
   begin
      Init(Key_5);
      Final_Sign(Message_5, 20, Tag_SHA_5);
      Trunc := To_Bytes(Tag_SHA_5)(1..16);

      Assert(Trunc = Tag_SHA ,"Symmetric Hmac SHA512 Test failed!");

   end Hmac_Test5;
------------------------------------------------------------------------------------
end Test.Hmac;
