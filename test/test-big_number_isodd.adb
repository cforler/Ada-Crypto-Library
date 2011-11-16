with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_IsOdd is

   -----------------------------------------------------------------------------
   ------------------------------ Type - Declaration ---------------------------
   -----------------------------------------------------------------------------
   
   package Big is new Crypto.Types.Big_Numbers(4096);
   use Big;
   use Big.Utils;
   
   use Big_Number_Constants;
   
   X_4096, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0, P, Q, R, Z : 
     Big_Unsigned;
   
   -----------------------------------------------------------------------------
   --------------------------------- Constants ---------------------------------
   -----------------------------------------------------------------------------

   procedure Constants is
   begin
      X_4096 := To_Big_Unsigned(Cons_4096);
      X_1025 := To_Big_Unsigned(Cons_1025);
      X_1024 := To_Big_Unsigned(Cons_1024);
      X_768  := To_Big_Unsigned(Cons_768);
      X_1 := To_Big_Unsigned("1");
      X_0 := To_Big_Unsigned("0");
   end Constants;

   -----------------------------------------------------------------------------
   -------------------------- Register Big_Number Test 1 -----------------------
   -----------------------------------------------------------------------------
	
   procedure Register_Tests(T : in out Big_Number_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Big_Number_Test1'Access,"Is Odd with 4096 Bit");
      Register_Routine(T, Big_Number_Test2'Access,"Is Odd with 1025 Bit");
      Register_Routine(T, Big_Number_Test3'Access,"Is Odd with 1024 Bit");
      Register_Routine(T, Big_Number_Test4'Access,"Is Odd with 768 Bit");
      Register_Routine(T, Big_Number_Test5'Access,"Is Odd with 1 Bit");
      Register_Routine(T, Big_Number_Test6'Access,"Is Odd with 0 Bit");
   end Register_Tests;

   -----------------------------------------------------------------------------
   ---------------------------- Name Big Number Tests --------------------------
   -----------------------------------------------------------------------------

   function Name(T : Big_Number_Test) return Test_String is
   begin
      return new String'("Big Number Tests");
   end Name;
   
   ------------------------------------------------------------------------
   ----------------------------- Start Tests ------------------------------
   ------------------------------------------------------------------------
   -------------------------------- Test 1 --------------------------------
   ------------------------------------------------------------------------
   
   procedure Big_Number_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_4096;
      Assert(Is_Odd(P), "Failed.");
   end Big_Number_Test1;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_1025;
      Assert(Is_Odd(P) = False, "Failed.");

   end Big_Number_Test2;

   -----------------------------------------------------------------------------
   ---------------------------------- Test 3 -----------------------------------
   -----------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_1024;
      Assert(Is_Odd(P), "Failed.");
   end Big_Number_Test3;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 4 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_768;
      Assert(Is_Odd(P) = False, "Failed.");
   end Big_Number_Test4;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 5 ----------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_1;
      Assert(Is_Odd(P), "Failed.");
   end Big_Number_Test5;

   -----------------------------------------------------------------------------
-------------------------------------- Test 6 ----------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      P := X_0;
      Assert(Is_Odd(P) = False, "Failed.");
   end Big_Number_Test6;

   ---------------------------------------------------------------------------

end Test.Big_Number_IsOdd;
