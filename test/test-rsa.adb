with AUnit.Assertions;
with Crypto.Asymmetric.RSA;
with Crypto.Types;
with Crypto.Types.Big_Numbers;


pragma Elaborate_All (Crypto.Asymmetric.RSA);

package body Test.RSA is

   ----------------------------------------------------------------------------
   --------------------------- Type - Declaration -----------------------------
   ----------------------------------------------------------------------------
   
   package RSA is new Crypto.Asymmetric.RSA(1024);
   use RSA;
   use RSA.Big;
   use RSA.Big.Utils;
   use Crypto.Types;
   use AUnit.Assertions;
   
   N_RSA:  constant RSA_Number :=
     (16#a8#, 16#b3#, 16#b2#, 16#84#, 16#af#, 16#8e#, 16#b5#, 16#0b#, 16#38#,
      16#70#, 16#34#, 16#a8#, 16#60#, 16#f1#, 16#46#, 16#c4#, 16#91#, 16#9f#,
      16#31#, 16#87#, 16#63#, 16#cd#, 16#6c#, 16#55#, 16#98#, 16#c8#, 16#ae#,
      16#48#, 16#11#, 16#a1#, 16#e0#, 16#ab#, 16#c4#, 16#c7#, 16#e0#, 16#b0#,
      16#82#, 16#d6#, 16#93#, 16#a5#, 16#e7#, 16#fc#, 16#ed#, 16#67#, 16#5c#, 
      16#f4#, 16#66#, 16#85#, 16#12#, 16#77#, 16#2c#, 16#0c#, 16#bc#, 16#64#, 
      16#a7#, 16#42#, 16#c6#, 16#c6#, 16#30#, 16#f5#, 16#33#, 16#c8#, 16#cc#,
      16#72#, 16#f6#, 16#2a#, 16#e8#, 16#33#, 16#c4#, 16#0b#, 16#f2#, 16#58#,
      16#42#, 16#e9#, 16#84#, 16#bb#, 16#78#, 16#bd#, 16#bf#, 16#97#, 16#c0#,
      16#10#, 16#7d#, 16#55#, 16#bd#, 16#b6#, 16#62#, 16#f5#, 16#c4#, 16#e0#,
      16#fa#, 16#b9#, 16#84#, 16#5c#, 16#b5#, 16#14#, 16#8e#, 16#f7#, 16#39#,
      16#2d#, 16#d3#, 16#aa#, 16#ff#, 16#93#, 16#ae#, 16#1e#, 16#6b#, 16#66#, 
      16#7b#, 16#b3#, 16#d4#, 16#24#, 16#76#, 16#16#, 16#d4#, 16#f5#, 16#ba#,
      16#10#, 16#d4#, 16#cf#, 16#d2#, 16#26#, 16#de#, 16#88#, 16#d3#, 16#9f#,
      16#16#, 16#fb#);     
   
   
   E_RSA : RSA_Number := (RSA_Number'Last => 1, RSA_Number'Last-2 => 1,
			  Others =>0);
   
   D_RSA : RSA_Number := 
     (16#53#, 16#33#, 16#9c#, 16#fd#, 16#b7#, 16#9f#, 16#c8#, 16#46#, 16#6a#, 
      16#65#, 16#5c#, 16#73#, 16#16#, 16#ac#, 16#a8#, 16#5c#, 16#55#, 16#fd#,
      16#8f#, 16#6d#, 16#d8#, 16#98#, 16#fd#, 16#af#, 16#11#, 16#95#, 16#17#,
      16#ef#, 16#4f#, 16#52#, 16#e8#, 16#fd#, 16#8e#, 16#25#, 16#8d#, 16#f9#, 
      16#3f#, 16#ee#, 16#18#, 16#0f#, 16#a0#, 16#e4#, 16#ab#, 16#29#, 16#69#,
      16#3c#, 16#d8#, 16#3b#, 16#15#, 16#2a#, 16#55#, 16#3d#, 16#4a#, 16#c4#,
      16#d1#, 16#81#, 16#2b#, 16#8b#, 16#9f#, 16#a5#, 16#af#, 16#0e#, 16#7f#,
      16#55#, 16#fe#, 16#73#, 16#04#, 16#df#, 16#41#, 16#57#, 16#09#, 16#26#,
      16#f3#, 16#31#, 16#1f#, 16#15#, 16#c4#, 16#d6#, 16#5a#, 16#73#, 16#2c#,
      16#48#, 16#31#, 16#16#, 16#ee#, 16#3d#, 16#3d#, 16#2d#, 16#0a#, 16#f3#,
      16#54#, 16#9a#, 16#d9#, 16#bf#, 16#7c#, 16#bf#, 16#b7#, 16#8a#, 16#d8#,
      16#84#, 16#f8#, 16#4d#, 16#5b#, 16#eb#, 16#04#, 16#72#, 16#4d#, 16#c7#,
      16#36#, 16#9b#, 16#31#, 16#de#, 16#f3#, 16#7d#, 16#0c#, 16#f5#, 16#39#,
      16#e9#, 16#cf#, 16#cd#, 16#d3#, 16#de#, 16#65#, 16#37#, 16#29#, 16#ea#,
      16#d5#, 16#D1#);
      
   N : constant Big_Unsigned  := To_Big_Unsigned(N_RSA);
   
   E : constant Big_Unsigned := To_Big_Unsigned("16#010001#");
   
   D : constant Big_Unsigned :=  To_Big_Unsigned(D_RSA);
   
   P : constant Big_Unsigned :=  To_Big_Unsigned
     ("16#d3_27_37_e7_26_7f_fe_13_41_b2_d5_c0_d1_50_a8_1b_58_6f_b3_13_2b_ed_2f"
	& "8d_52_62_86_4a_9c_b9_f3_0a_f3_8b_e4_48_59_8d_41_3a_17_2e_fb_80_2c"
	& "21_ac_f1_c1_1c_52_0c_2f_26_a4_71_dc_ad_21_2e_ac_7c_a3_9d#");
   
   Q : constant Big_Unsigned := To_Big_Unsigned
     ("16#cc_88_53_d1_d5_4d_a6_30_fa_c0_04_f4_71_f2_81_c7_b8_98_2d_82_24_a4_90"
	& "ed_be_b3_3d_3e_3d_5c_c9_3c_47_65_70_3d_1d_d7_91_64_2f_1f_11_6a_0d"
	& "d8_52_be_24_19_b2_af_72_bf_e9_a0_30_e8_60_b0_28_8b_5d_77#");
  
   Phi: constant Big_Unsigned := (P-1) * (Q-1);


   --  References values for DP, DQ and QInv generated from P and Q
   --  using the tools at:
   --  http://www.mobilefish.com/services/rsa_key_generation/rsa_key_generation.php
   DP   : constant Big_Unsigned := To_Big_Unsigned
     ("16#0e_12_bf_17_18_e9_ce_f5" &
         "59_9b_a1_c3_88_2f_e8_04" &
	 "6a_90_87_4e_ef_ce_8f_2c" &
	 "cc_20_e4_f2_74_1f_b0_a3" &
	 "3a_38_48_ae_c9_c9_30_5f" &
	 "be_cb_d2_d7_68_19_96_7d" &
	 "46_71_ac_c6_43_1e_40_37" &
	 "96_8d_b3_78_78_e6_95_c1#");

   DQ   : constant Big_Unsigned := To_Big_Unsigned
     ("16#95_29_7b_0f_95_a2_fa_67" &
         "d0_07_07_d6_09_df_d4_fc" & 
	 "05_c8_9d_af_c2_ef_6d_6e" &
	 "a5_5b_ec_77_1e_a3_33_73" &
	 "4d_92_51_e7_90_82_ec_da" &
	 "86_6e_fe_f1_3c_45_9e_1a" &
	 "63_13_86_b7_e3_54_c8_99" &
	 "f5_f1_12_ca_85_d7_15_83#");


   QInv : constant Big_Unsigned := To_Big_Unsigned
     ("16#4f_45_6c_50_24_93_bd_c0" &
 	 "ed_2a_b7_56_a3_a6_ed_4d" & 
	 "67_35_2a_69_7d_42_16_e9" &
	 "32_12_b1_27_a6_3d_54_11" &
	 "ce_6f_a9_8d_5d_be_fd_73" &
	 "26_3e_37_28_14_27_43_81" &
	 "81_66_ed_7d_d6_36_87_dd" &
	 "2a_8c_a1_d2_f4_fb_d8_e1#");

   Mess: constant RSA_Number :=
     (16#43#, 16#79#, 16#cf#, 16#00#, 16#3c#, 16#3b#, 16#74#, 16#0d#,
      16#d6#, 16#34#, 16#00#, 16#0c#, 16#4d#, 16#03#, 16#43#, 16#98#,
      16#29#, 16#3c#, 16#39#, 16#3e#, 16#9c#, 16#98#, 16#5f#, 16#23#,
      16#f7#, 16#8e#, 16#00#, 16#49#, 16#cd#, 16#f3#, 16#2f#, 16#ce#,
      16#24#, 16#9f#, 16#8e#, 16#c3#, 16#2d#, 16#6b#, 16#a3#, 16#f7#,
      16#16#, 16#4a#, 16#b6#, 16#4e#, 16#00#, 16#b1#, 16#33#, 16#4e#,
      16#3c#, 16#81#, 16#c0#, 16#4c#, 16#2b#, 16#d1#, 16#29#, 16#2c#,
      16#00#, 16#7d#, 16#04#, 16#74#, 16#5b#, 16#e3#, 16#f0#, 16#c9#,
      16#43#, 16#79#, 16#cf#, 16#00#, 16#3c#, 16#3b#, 16#74#, 16#0d#,
      16#d6#, 16#34#, 16#00#, 16#0c#, 16#4d#, 16#03#, 16#43#, 16#98#,
      16#29#, 16#3c#, 16#39#, 16#3e#, 16#9c#, 16#98#, 16#5f#, 16#23#,
      16#f7#, 16#8e#, 16#00#, 16#49#, 16#cd#, 16#f3#, 16#2f#, 16#ce#,
      16#24#, 16#9f#, 16#8e#, 16#c3#, 16#2d#, 16#6b#, 16#a3#, 16#f7#,
      16#16#, 16#4a#, 16#b6#, 16#4e#, 16#00#, 16#b1#, 16#33#, 16#4e#,
      16#3c#, 16#81#, 16#c0#, 16#4c#, 16#2b#, 16#d1#, 16#29#, 16#2c#,
      16#00#, 16#7d#, 16#04#, 16#74#, 16#5b#, 16#e3#, 16#f0#, 16#c9#);

     
   ----------------------------------------------------------------------------
  --------------------------- Aux Methods -------------------------------------
   ----------------------------------------------------------------------------
         
   procedure Set_Keypair(Pk : out Public_Key_RSA; Sk : out Private_Key_RSA) is
   begin
      Set_Public_Key(N, E, Pk);
      Set_Private_Key(N, D, P, Q, Phi, Sk);
   end Set_Keypair;
   
   ----------------------------------------------------------------------------
   
   function RSA_Enc_Dec_Test(Msg : RSA_Number) return Boolean is
      SK : Private_Key_RSA;
      Pk : Public_Key_RSA;
      Plaintext  : RSA_Number;
      Ciphertext : RSA_Number;
   begin
      Set_Keypair(Pk, Sk);
      Encrypt(pk ,Msg , Ciphertext);
      Decrypt(Sk ,Ciphertext, Plaintext);
      return Plaintext = Msg;
   end RSA_Enc_Dec_Test;
      
   ----------------------------------------------------------------------------
   -------------------------- Register RSA Test 1 -----------------------------
   ----------------------------------------------------------------------------

   procedure Register_Tests(T : in out RSA_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Verify_Key_Pair'Access,
		       "RSA: verify keypair test.");
      Register_Routine(T, Test_OAEP'Access,"RSA: OAEP test.");
      Register_Routine(T, Test_Gen_Keypair'Access,
		       "RSA: generate keypair test.");
      Register_Routine(T, Test_Get_Public_key'Access,
		       "RSA: get public key test");
      Register_Routine(T, Test_Encryption'Access,
		       "RSA: encryption test.");
      Register_Routine(T, Test_Zero_Encryption'Access,
		       "RSA: zero encryption test.");
      Register_Routine(T, Test_Get_Private_Key'Access,
		       "RSA: Get Public/Private Key Test.");
   end Register_Tests;

   ----------------------------------------------------------------------------
   ------------------------------ Name RSA Test -------------------------------
   ----------------------------------------------------------------------------

   function Name(T : RSA_Test) return Test_String is
   begin
      return new String'("RSA Test");
   end Name;

--------------------------------- Start Tests --------------------------------
	
   procedure Test_Verify_Key_Pair(T : in out Test_Cases.Test_Case'Class) is
      SK : Private_Key_RSA;
      Pk : Public_Key_RSA;
   begin
      Set_Keypair(Pk,Sk);
      Assert(Verify_Key_Pair(SK, PK), "RSA verifying Key failed.");
   end Test_Verify_Key_Pair;

   ---------------------------------------------------------------------------
   
   procedure Test_OAEP(T : in out Test_Cases.Test_Case'Class) is
      SK  : Private_Key_RSA;
      Pk  : Public_Key_RSA;
      Msg : constant Bytes := 
	(16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#, 
	 16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#, 16#00#,
	 16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#,
	 16#99#, 16#aa#, 16#bb#, 16#cc#, 16#dd#, 16#ee#, 16#ff#, 16#de#,
	 16#ad#, 16#be#, 16#ef#, 16#ff#);   
   begin
      Set_Keypair(Pk,Sk);
      declare
         Ciphertext: constant Bytes := OAEP_Encrypt(Pk, Msg);
         Plaintext:  constant Bytes := OAEP_Decrypt(Sk, Ciphertext);
      begin
	 Assert(Msg = Plaintext, "OEAP RSA failed.");
      end;
   end Test_OAEP;

   ---------------------------------------------------------------------------

   procedure Test_Gen_Keypair(T : in out Test_Cases.Test_Case'Class) is
      SK : Private_Key_RSA;
      Pk : Public_Key_RSA;
   begin
      Gen_Key(Pk, Sk);
      Assert(Verify_Key_Pair(Sk, Pk), "RSA verifying Key failed.");
   end Test_Gen_Keypair;
   

-------------------------------------------------------------------------------

   procedure Test_Get_Public_Key(T : in out Test_Cases.Test_Case'Class) is
      Pk : Public_Key_RSA; 
      N_New, E_New : RSA_Number;
   begin
      Set_Public_Key(N, E, Pk);
      Get_Public_Key(Pk, N_new, E_New);
      
      Assert(N_New = N_RSA and E_New = E_RSA, "RSA getting public key failed.");
   end Test_Get_Public_Key;

   ---------------------------------------------------------------------------

   procedure Test_Encryption(T : in out Test_Cases.Test_Case'Class) is
      Msg : constant RSA_Number := (others => 16#7f#);
   begin
      Assert(RSA_Enc_Dec_Test(Msg), "RSA encrypting and decrypting failed.");
   end Test_Encryption;

   --------------------------------------------------------------------------

   procedure Test_Zero_Encryption(T : in out Test_Cases.Test_Case'Class) is
      Msg : constant RSA_Number := (others => 16#0#);
   begin
      Assert(RSA_Enc_Dec_Test(Msg),
	     "RSA encryption/decryption of zero failed.");
   end Test_Zero_Encryption;

   -------------------------------------------------------------------------

   procedure Test_Get_Private_Key(T : in out Test_Cases.Test_Case'Class) is
      NComp, DComp, PhiComp, PComp, QComp : RSA_Number;
      DPComp, DQComp, QInvComp : RSA_Number;
      Sk  : Private_Key_RSA;
   begin
      Set_Private_Key(N, D, P, Q, Phi, Sk);
      RSA.Get_Private_Key(Sk, NComp, DComp, PComp, QComp, PhiComp,
			 DPComp, DQComp, QInvComp);

      Assert(N    = To_Big_Unsigned(NComp) and
	     D    = To_Big_Unsigned(DComp) and  
	     P    = To_Big_Unsigned(PComp) and
	     Q    = To_Big_Unsigned(QComp) and
	     Phi  = To_Big_Unsigned(PhiComp) and
	     DP   = To_Big_Unsigned(DPComp) and
	     DQ   = To_Big_Unsigned(DQComp) and
	     QInv = To_Big_Unsigned(QInvComp),
	     "RSA_new Get Public/Private Key failed.");
   end Test_Get_Private_Key;
   
end Test.RSA;
