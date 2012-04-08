with AUnit.Assertions;
with Crypto.Types;
with Crypto.Random;
with Text_IO;
with Crypto.Types.Big_Numbers;
with Crypto.Types;


package body Test.Big_numbers_division is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    use Big.Mod_Utils;
    use Crypto.Types;

   -----------------------------------------------------------------------------
   ------------------------ Register Big_numbers_division Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Big_numbers_division_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Big_numbers_division_Test1'Access,"Big_numbers_division Known Answer Test 1");

   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Big_numbers_division Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Big_numbers_division_Test) return Test_String is
   begin
      return new String'("Big_numbers_division Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Big_numbers_division_Test1(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

       A: Mod_Type;
       B, C, D: Big_Unsigned;


   begin
      A := 222222;
      B := To_Big_Unsigned("111111");
      C := To_Big_Unsigned("2");
      D := A/B;
      Assert(D = C , "Big_numbers_division Known Answer Test 1 failed");
   end Big_numbers_division_Test1;

   -----------------------------------------------------------------------------


end Test.Big_numbers_division;
