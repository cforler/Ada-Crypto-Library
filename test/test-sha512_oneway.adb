with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.SHA512.Oneway;
with Crypto.Types;
with Crypto.Types.Random;
with Text_IO;
use Crypto.Types;

package body Test.SHA512_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register SHA512 Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out SHA512_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, SHA512_Oneway_Test1'Access,"SHA512 Oneway Determinism Test");
      Register_Routine(T, SHA512_Oneway_Test2'Access,"SHA512 Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name SHA512 Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : SHA512_Oneway_Test) return Test_String is
   begin
      return new String'("SHA512 Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA512_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA512.Oneway;
      One_Way_Key : Crypto.Types.DW_Block256;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.SHA512.Oneway.Cipherkey_SHA512;
      One_Way_Plaintext : Crypto.Types.DW_Block512;
      One_Way_Ciphertext : Crypto.Types.DW_Block512;
      One_Way_Ciphertext2 : Crypto.Types.DW_Block512;
      Is_distinct : Boolean := true;
      random_byte1 : 	Crypto.Types.Byte;
      random_byte2 : 	Crypto.Types.Byte;
      random_byte3 : 	Crypto.Types.Byte;
      random_byte4 : 	Crypto.Types.Byte;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
            Crypto.Types.Random.Read(random_byte1);
            Crypto.Types.Random.Read(random_byte2);
            Crypto.Types.Random.Read(random_byte3);
            Crypto.Types.Random.Read(random_byte4);
            One_Way_Key(i):=To_DWord((random_byte1, random_byte2, random_byte3, random_byte4,
              			      random_byte1, random_byte2, random_byte3, random_byte4));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
           Crypto.Types.Random.Read(random_byte1);
            Crypto.Types.Random.Read(random_byte2);
            Crypto.Types.Random.Read(random_byte3);
            Crypto.Types.Random.Read(random_byte4);
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

      Assert(Is_distinct = true, "SHA512 Oneway Determinism Test failed");
   end SHA512_Oneway_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA512_Oneway_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA512.Oneway;
      use Crypto.Types;

      One_Way_Plaintext: DW_Block512 := (16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#);

      One_Way_Test_Ciphertext: DW_Block512 :=  (16#2E_B6_78_36_C3_75_36_31#, 16#212046393F7739E7#,
                                               	16#9F_E9_8A_07_21_04_8F_5B#,  16#70_6B_45_87_92_29_42_FB#,
                                         	16#CC_BA_A0_9A_AB_63_70_14#, 16#11_8D_7D_96_4A_30_6F_EA#,
                                         	16#5A_30_07_36_3C_13_38_91#, 16#83_D9_DB_BC_78_23_83_DB#);

      One_Way_Key: DW_Block256 := 	(16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#,
                                         16#01_02_A1_A2_1_02_A1_A2#, 16#01_02_A1_A2_1_02_A1_A2#);

      One_Way_Cipherkey: Cipherkey_SHA512;
      One_Way_Ciphertext: DW_Block512;

   begin

      Prepare_Key(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);

      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "SHA512 Oneway Known Answer Test failed");
   end SHA512_Oneway_Test2;

------------------------------------------------------------------------------------

end Test.SHA512_Oneway;
