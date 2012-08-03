with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.SHA256.Oneway;
with Crypto.Types;
with Crypto.Types.Random;
with Text_IO;
use Crypto.Types;

package body Test.SHA256_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register SHA256 Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out SHA256_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, SHA256_Oneway_Test1'Access,"SHA256 Oneway Determinism Test");
      Register_Routine(T, SHA256_Oneway_Test2'Access,"SHA256 Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name SHA256 Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : SHA256_Oneway_Test) return Test_String is
   begin
      return new String'("SHA256 Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA256_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA256.Oneway;
      One_Way_Key : Crypto.Types.W_Block256;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.SHA256.Oneway.Cipherkey_SHA256;
      One_Way_Plaintext : Crypto.Types.W_Block256;
      One_Way_Ciphertext : Crypto.Types.W_Block256;
      One_Way_Ciphertext2 : Crypto.Types.W_Block256;
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
            One_Way_Key(i):=To_Word((random_byte1, random_byte2, random_byte3, random_byte4));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
           Crypto.Types.Random.Read(random_byte1);
            Crypto.Types.Random.Read(random_byte2);
            Crypto.Types.Random.Read(random_byte3);
            Crypto.Types.Random.Read(random_byte4);
            One_Way_Plaintext(i):=To_Word((random_byte1, random_byte2, random_byte3, random_byte4));
      	end loop;
      	Prepare_Key(One_Way_Key, One_Way_Cipherkey);
      	Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct, "SHA256 Oneway Determinism Test failed");
   end SHA256_Oneway_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure SHA256_Oneway_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.SHA256.Oneway;

      One_Way_Plaintext: W_Block256 := (16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#,
                                       16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#);

      One_Way_Test_Ciphertext: W_Block256 := (	16#55_F8_F4_59#,
						16#4D_9F_EB_3E#,
						16#1C_A4_D4_7A#,
						16#59_B8_9B_C3#,
						16#77_CB_47_5A#,
						16#5E_A3_45_E5#,
						16#06_F5_FD_82#,
						16#FB_0D_ED_33#);

      One_Way_Key: W_Block256 := (16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#,
                                 16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#, 16#01_02_A1_A2#);

      One_Way_Cipherkey: Cipherkey_SHA256;
      One_Way_Ciphertext: W_Block256;

   begin

      Prepare_Key(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);

      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "SHA256 Oneway Known Answer Test failed");
   end SHA256_Oneway_Test2;

------------------------------------------------------------------------------------

end Test.SHA256_Oneway;
