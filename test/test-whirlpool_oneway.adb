with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Whirlpool.Oneway;
with Crypto.Types;
with Crypto.Random;
with Text_IO;

package body Test.Whirlpool_Oneway is
use Crypto.Types;

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------


------------------------------------------------------------------------------------
---------------------------- Register Whirlpool Oneway Test 1 ----------------------------
------------------------------------------------------------------------------------

   procedure Register_Tests(T : in out Whirlpool_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Whirlpool_Oneway_Test1'Access,"Whirlpool Oneway Determinism Test");
   end Register_Tests;

------------------------------------------------------------------------------------
------------------------------ Name Whirlpool Oneway Test ------------------------------
------------------------------------------------------------------------------------

   function Name(T : Whirlpool_Oneway_Test) return Test_String is
   begin
      return new String'("Whirlpool Oneway Test");
   end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Whirlpool_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.Whirlpool.Oneway;
      
      One_Way_Key         :  Crypto.Types.DW_Block256 := (16#DEAD_FEFE_ABBA_AFFE#, 16#C0DE_F4EE_3137_B0DE#, 16#8BAD_F00D#, 16#CAFE_BABE_CAFE_D00D#);
      One_Way_Cipherkey   :  Cipherkey_Whirlpool;
      One_Way_Plaintext   :  DW_Block256 := (16#01_02_03_04_05_06_07_08#, 16#09_0a_0b_0c_0d_0e_0f_00#, 16#DEAD_BABE_DEAD_BEEF#, 16#DEAD_DEAD_DEAD_FA11#);
      One_Way_Ciphertext  :  DW_Block256;
      One_Way_Ciphertext2 :  DW_Block256;
   begin
         Prepare_Key(One_Way_Key, One_Way_Cipherkey);
         Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
         Encrypt(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);

         Assert(One_Way_Ciphertext= One_Way_Ciphertext2 , "Whirlpool Oneway Determinism Test failed");
   end Whirlpool_Oneway_Test1;

------------------------------------------------------------------------------------


end Test.Whirlpool_Oneway;
