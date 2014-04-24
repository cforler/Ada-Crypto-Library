with AUnit.Assertions; 
with Ada.Text_IO;
with Crypto.Symmetric.MAC.HMAC_SHA256;
with Crypto.Types;

package body Test.SHA256_MAC is
   use Crypto.Types;
   
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	package DIO is new Ada.Text_IO.Modular_IO (DWord);
	use DIO;
	
	Key1: W_Block512 := (0 => 16#0b_0b_0b_0b#, 1 => 16#0b_0b_0b_0b#,
                         2 => 16#0b_0b_0b_0b#, 3 => 16#0b_0b_0b_0b#,
                         4 => 16#0b_0b_0b_0b#, others  => 0);
    
    Message1: W_Block512 := (0 => 16#48_69_20_54#, 1 => 16#68_65_72_65#,
                             others => 0);
    
    Key2:  W_Block512 := (0 => 16#4a_65_66_65#, others  => 0);
   
    Message2: W_Block512 := ( 0 => 16#77_68_61_74#, 1 => 16#20_64_6f_20#,
                              2 => 16#79_61_20_77#, 3 => 16#61_6e_74_20#,
                              4 => 16#66_6f_72_20#, 5 => 16#6e_6f_74_68#,
                              6 => 16#69_6e_67_3f#, others  => 0);
    
    Key3: W_Block512 := (0 => 16#Aa_Aa_Aa_Aa#, 1 => 16#Aa_Aa_Aa_Aa#,
                         2 => 16#Aa_Aa_Aa_Aa#, 3 => 16#Aa_Aa_Aa_Aa#,
                         4 => 16#Aa_Aa_Aa_Aa#, others => 0);
    
    Message3: W_Block512 := (0..11 => 16#Dd_Dd_Dd_Dd#, 12 => 16#Dd_Dd_00_00#,
                             others => 0);

    
    Temp: W_Block256 := (16#B0_34_4c_61#, 16#D8_Db_38_53#, 16#5c_A8_Af_Ce#,
	                     16#Af_0b_F1_2b#, 16#88_1d_C2_00#, 16#C9_83_3d_A7#,
                         16#26_E9_37_6c#, 16#2e_32_Cf_F7#);
    
    Tag: W_Block256;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register SHA256 MAC Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out HMAC_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA256_MAC_Test1'Access,"SHA256_MAC_Test1.");
		Register_Routine(T, SHA256_MAC_Test2'Access,"SHA256_MAC_Test2.");
		Register_Routine(T, SHA256_MAC_Test3'Access,"SHA256_`MAC_Test3.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------- Name SHA256 MAC Test -------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : HMAC_Test) return Test_String is
	begin
		return new String'("SHA256 MAC Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA256_MAC_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA256;

      Context : HMAC_Context;
      
   begin

   	   Context.Init(Key1);
   	   Context.Final_Sign(Message1, 8, Tag);

       Assert((Tag = Temp) or (Context.Final_Verify(Message1, 8, Tag)), "Final Signature with SHA256 MAC failed.");

   end SHA256_MAC_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA256_MAC_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA256;
      
      Context : HMAC_Context;

   begin
   	   
   	   Temp := (16#5b_Dc_C1_46#, 16#Bf_60_75_4e#, 16#6a_04_24_26#,
                16#08_95_75_C7#, 16#5a_00_3f_08#, 16#9d_27_39_83#,
                16#9d_Ec_58_B9#, 16#64_Ec_38_43#);
   	   Context.Init(Key2);
   	   Context.Final_Sign(Message2, 28, Tag);

       Assert((Tag = Temp) or (Context.Final_Verify(Message2, 28, Tag)), "Final Signature with SHA256 MAC failed.");

   end SHA256_MAC_Test2;
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA256_MAC_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA256;

      Context : HMAC_Context;
      
   begin
   	   
   	   Temp := (16#77_3e_A9_1e#, 16#36_80_0e_46#, 16#85_4d_B8_Eb#,
                16#D0_91_81_A7#, 16#29_59_09_8b#, 16#3e_F8_C1_22#,
                16#D9_63_55_14#, 16#Ce_D5_65_Fe#);
   	   Context.Init(Key3);
   	   Context.Final_Sign(Message3, 50, Tag);

       Assert((Tag = Temp) or (Context.Final_Verify(Message3, 50, Tag)), "Final Signature with SHA256 MAC failed.");
   
   end SHA256_MAC_Test3;

------------------------------------------------------------------------------------

end Test.SHA256_MAC;
