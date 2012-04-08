with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.MMH;
with Crypto.Types;
with Crypto.Random;
with Text_IO;
use Crypto.Types;

package body Test.MMH is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register MMH Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out MMH_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, MMH_Test1'Access,"MMH Known Answer Test 1");
      Register_Routine(T, MMH_Test2'Access,"MMH Known Answer Test 2");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name MMH Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : MMH_Test) return Test_String is
   begin
      return new String'("MMH Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure MMH_Test1(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.MMH;
      MMH_Key : crypto.Types.Word := (16#01_02_A1_A2#);
      MMH_Message : crypto.Types.Word := (16#03_04_A3_A4#);
      MMH_Hash : crypto.Types.Word := (16#05_06_A5_A6#);
      MMH_Hash_Test : crypto.Types.Word := (16#BE_F3_F5_1C#);

   begin
      Hash(MMH_Key, MMH_Message, MMH_Hash);

      Assert(MMH_Hash = MMH_Hash_Test, "MMH Known Answer Test 1 failed");
   end MMH_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure MMH_Test2(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.MMH;
      MMH_Key : crypto.Types.Words := (16#01_02_A1_A2#, 16#02_04_A4_A1#, 16#02_04_A4_A1#, 16#02_04_A4_A1#);
      MMH_Message : crypto.Types.Words := ((16#03_04_03_A4#), (16#A3_04_A3_A4#), (16#02_04_A4_A1#), (16#02_04_A4_A1#));
      MMH_Hash : crypto.Types.Word;
      MMH_Hash_Test : crypto.Types.Word := (16#81_4A_FE_C0#);

   begin
      Hash(MMH_Key, MMH_Message, MMH_Hash);
      Assert(MMH_Hash = MMH_Hash_Test, "MMH Known Answer Test 2 failed");
   end MMH_Test2;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------

end Test.MMH;
