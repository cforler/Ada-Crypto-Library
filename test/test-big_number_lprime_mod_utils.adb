with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);

package body Test.Big_Number_LPrime_Mod_Utils is

   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
	
   package Big is new Crypto.Types.Big_Numbers(4096);
   use Big;
   use Big.Utils;
   use Big.Mod_Utils;
   use Big_Number_Constants;	
   
   A, X : Big_Unsigned;
   B : Boolean;
	
   -----------------------------------------------------------------------------
   -------------------------------- Constants ----------------------------------
   ----------------------------------------------------------------------------

   procedure Results is
   begin
      null;
   end Results;

   -----------------------------------------------------------------------------
   -------------------- Register Big_Number_Mod_Utils Tests --------------------
   -----------------------------------------------------------------------------
	
   procedure Register_Tests(T : in out Big_Number_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Big_Number_Mod_Utils_Test1'Access,"Get Prime with Modulo N = 2^4096-1.");
   end Register_Tests;

   -----------------------------------------------------------------------------
   ------------------------ Name Big Number Prime Test -------------------------
   -----------------------------------------------------------------------------

   function Name(T : Big_Number_Test) return Test_String is
   begin
      return new String'("Big Number Mod Utils Tests");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   -------------------------------------- Test 1 -------------------------------
   -----------------------------------------------------------------------------
   
   procedure Big_Number_Mod_Utils_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Results;
      A := To_Big_Unsigned("20000");
      X := Get_Prime(A);
      B := Looks_Like_A_Prime(X);
      Assert(B, "Not Prime");
   end Big_Number_Mod_Utils_Test1;

      --------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

--   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions; 
--   begin
--
--	   X := Get_Prime(X_3812);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--   
--   end Big_Number_Mod_Utils_Test2;
--	   
--------------------------------------------------------------------------------------
---------------------------------------- Test 3 --------------------------------------
--------------------------------------------------------------------------------------
--
--   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions; 
--   begin
--   	   
--	   X := Get_Prime(X_2048);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--   
--   end Big_Number_Mod_Utils_Test3;
--
--------------------------------------------------------------------------------------
---------------------------------------- Test 4 --------------------------------------
--------------------------------------------------------------------------------------
--
--   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions; 
--   begin
--   	   
--	   X := Get_Prime(X_1025);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--   
--   end Big_Number_Mod_Utils_Test4;
--
--------------------------------------------------------------------------------------
---------------------------------------- Test 5 --------------------------------------
--------------------------------------------------------------------------------------
--
--   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions; 
--   begin
--   	   
--	   X := Get_Prime(X_1024);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--   
--   end Big_Number_Mod_Utils_Test5;
--
--------------------------------------------------------------------------------------
---------------------------------------- Test 6 --------------------------------------
--------------------------------------------------------------------------------------
--
--   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions; 
--   begin
--  	   
--	   X := Get_Prime(X_768);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--	   
--   end Big_Number_Mod_Utils_Test6;
--
--------------------------------------------------------------------------------------
---------------------------------------- Test 7 --------------------------------------
--------------------------------------------------------------------------------------
--
--   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
--      use AUnit.Assertions;
--   begin
--
--	   X := Get_Prime(X_582);
--	   B := Looks_Like_A_Prime(X);
--	   Assert(B = True, "Not Prime");
--   
--   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_LPrime_Mod_Utils;
