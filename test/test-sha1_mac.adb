with AUnit.Assertions; 
with Ada.Text_IO;
with Crypto.Symmetric.MAC.HMAC_SHA1;
with Crypto.Types;

package body Test.SHA1_MAC is
   use Crypto.Types;
   
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	package DIO is new Ada.Text_IO.Modular_IO (DWord);
	use DIO;
	
	--Test vectors from RFC 4231 and RFC 2202
	Key1: W_Block512 := (0 => 16#0b_0b_0b_0b#, 1 => 16#0b_0b_0b_0b#,
                         2 => 16#0b_0b_0b_0b#, 3 => 16#0b_0b_0b_0b#,
                         4 => 16#0b_0b_0b_0b#, others  => 0);
   
   	--"Hi There"
    Message1: W_Block512 := (0 => 16#48_69_20_54#, 1 => 16#68_65_72_65#,
                             others => 0);
    
    --"Jefe"
    Key2:  W_Block512 := (0 => 16#4a_65_66_65#, others  => 0);
   
    --"What do ya want for nothing?"
    Message2: W_Block512 := ( 0 => 16#77_68_61_74#, 1 => 16#20_64_6f_20#,
                              2 => 16#79_61_20_77#, 3 => 16#61_6e_74_20#,
                              4 => 16#66_6f_72_20#, 5 => 16#6e_6f_74_68#,
                              6 => 16#69_6e_67_3f#, others  => 0);
    
    Key3: W_Block512 := (0 => 16#Aa_Aa_Aa_Aa#, 1 => 16#Aa_Aa_Aa_Aa#,
                         2 => 16#Aa_Aa_Aa_Aa#, 3 => 16#Aa_Aa_Aa_Aa#,
                         4 => 16#Aa_Aa_Aa_Aa#, others => 0);
    
    Message3: W_Block512 := (0..11 => 16#Dd_Dd_Dd_Dd#, 12 => 16#Dd_Dd_00_00#,
                             others => 0);

    Temp: W_Block160 := (16#B6_17_31_86#, 16#55_05_72_64#, 16#E2_8b_C0_B6#,
                         16#Fb_37_8c_8e#, 16#F1_46_Be_00#);
    
    Tag: W_Block160;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register SHA1 MAC Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out HMAC_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA1_MAC_Test1'Access,"SHA1_MAC_Test1.");
		Register_Routine(T, SHA1_MAC_Test2'Access,"SHA1_MAC_Test2.");
		Register_Routine(T, SHA1_MAC_Test3'Access,"SHA1_MAC_Test3.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Name SHA1 MAC Test --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : HMAC_Test) return Test_String is
	begin
		return new String'("SHA1 MAC Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA1_MAC_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA1;

   begin

   	   Init(Key1);
   	   Final_Sign(Message1, 8, Tag);

       Assert((Tag = Temp) or (Final_Verify(Message1, 8, Tag)), "Final Signature with SHA1 MAC failed.");

   end SHA1_MAC_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA1_MAC_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA1;

   begin
   	   
   	   Temp := (16#Ef_Fc_Df_6a#, 16#E5_Eb_2f_A2#, 16#D2_74_16_D5#,
               16#F1_84_Df_9c#, 16#25_9a_7c_79#);
   	   Init(Key2);
   	   Final_Sign(Message2, 28, Tag);

       Assert((Tag = Temp) or (Final_Verify(Message2, 28, Tag)), "Final Signature with SHA1 MAC failed.");

   end SHA1_MAC_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA1_MAC_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.MAC.HMAC_SHA1;

   begin
   	   
   	   Temp := (16#12_5d_73_42#, 16#B9_Ac_11_Cd#, 16#91_A3_9a_F4#,
               16#8a_A1_7b_4f#, 16#63_F1_75_D3#);
   	   Init(Key3);
   	   Final_Sign(Message3, 50, Tag);

       Assert((Tag = Temp) or (Final_Verify(Message3, 50, Tag)), "Final Signature with SHA1 MAC failed.");
   
   end SHA1_MAC_Test3;

------------------------------------------------------------------------------------

end Test.SHA1_MAC;
