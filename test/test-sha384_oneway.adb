with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.SHA384.Oneway;
with Crypto.Types;
with Crypto.Random;
with Text_IO;
use Crypto.Types;

package body Test.SHA384_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register SHA384 Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out SHA384_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, SHA384_Oneway_Test1'Access,"SHA384 Oneway Determinism Test");
      Register_Routine(T, SHA384_Oneway_Test2'Access,"SHA384 Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name SHA384 Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : SHA384_Oneway_Test) return Test_String is
   begin
      return new String'("SHA384 Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA384_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA384.Oneway;
      One_Way_Key : Crypto.Types.DW_Block256;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.SHA384.Oneway.Cipherkey_SHA384;
      One_Way_Plaintext : Crypto.Types.DW_Block384;
      One_Way_Ciphertext : Crypto.Types.DW_Block384;
      One_Way_Ciphertext2 : Crypto.Types.DW_Block384;
      Is_distinct : Boolean := true;
      random_byte1 : 	Crypto.Types.Byte;
      random_byte2 : 	Crypto.Types.Byte;
      random_byte3 : 	Crypto.Types.Byte;
      random_byte4 : 	Crypto.Types.Byte;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
            Crypto.Random.Read(random_byte1);
            Crypto.Random.Read(random_byte2);
            Crypto.Random.Read(random_byte3);
            Crypto.Random.Read(random_byte4);
            One_Way_Key(i):=To_DWord((random_byte1, random_byte2, random_byte3, random_byte4,
              			      random_byte1, random_byte2, random_byte3, random_byte4));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
           Crypto.Random.Read(random_byte1);
            Crypto.Random.Read(random_byte2);
            Crypto.Random.Read(random_byte3);
            Crypto.Random.Read(random_byte4);
            One_Way_Plaintext(i):=To_DWord((random_byte1, random_byte2, random_byte3, random_byte4,
             				    random_byte1, random_byte2, random_byte3, random_byte4));
      	end loop;
         Prepare_Key(One_Way_Key, One_Way_Cipherkey);
      	Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct = true, "SHA384 Oneway Determinism Test failed");
   end SHA384_Oneway_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA384_Oneway_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA384.Oneway;
      use Crypto.Types;

      One_Way_Plaintext: DW_Block384 := (16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#);

      One_Way_Test_Ciphertext: DW_Block384 := (16#A7_63_EE_0B_6F_0A_6F_40#, 16#D9_F9_A0_49_13_32_DF_8E#,
                                         16#EC_52_48_D6_F4_A7_66_F5#, 16#C4_31_AF_EB_62_0D_2F_0B#,
                                         16#86_47_A5_67_0B_37_1E_B8#, 16#98_6C_76_B9_FC_71_7C_4F#);

      One_Way_Key: DW_Block256 := 	(16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#);

      One_Way_Cipherkey: Cipherkey_SHA384;
      One_Way_Ciphertext: DW_Block384;

   begin

      Prepare_Key(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);

      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "SHA384 Oneway Known Answer Test failed");
   end SHA384_Oneway_Test2;

------------------------------------------------------------------------------------

end Test.SHA384_Oneway;
