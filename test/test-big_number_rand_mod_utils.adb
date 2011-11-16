with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Ada.Text_IO; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
package body Test.Big_Number_Rand_Mod_Utils is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    use Big.Mod_Utils;
	
	use Ada.Text_IO;
	use Big_Number_Constants;	

    X_4096, X_4095, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0: Big_Unsigned;
	A, N, X:  Big_Unsigned;
	
------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Results is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_2048 := To_Big_Unsigned(Cons_2048);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_582  := To_Big_Unsigned(Cons_582);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");
		
	end Results;

------------------------------------------------------------------------------------
----------------------- Register Big_Number_Mod_Utils Tests ------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Utils_Test1'Access,"Get Radnom with Modulo N = 2^4096-1.");
		Register_Routine(T, Big_Number_Mod_Utils_Test2'Access,"Get Radnom with Modulo N = 2^3812.");
		Register_Routine(T, Big_Number_Mod_Utils_Test3'Access,"Get Radnom with Modulo N = 2^2048.");
		Register_Routine(T, Big_Number_Mod_Utils_Test4'Access,"Get Radnom with Modulo N = 2^1025.");
		Register_Routine(T, Big_Number_Mod_Utils_Test5'Access,"Get Radnom with Modulo N = 2^1024.");
		Register_Routine(T, Big_Number_Mod_Utils_Test6'Access,"Get Radnom with Modulo N = 2^768.");
		Register_Routine(T, Big_Number_Mod_Utils_Test7'Access,"Get Radnom with Modulo N = 2^0.");

	end Register_Tests;

------------------------------------------------------------------------------------
-------------------------- Name Big_Number_Mod_Utils Test --------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number Mod Utils Tests");
	end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   Results;

	   X := Get_Random(X_4096);
	   N := Get_Random(X_4096);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_4096);
	   N := Get_Random(X_4096);
	   Assert(X /= N, "Not Random");
   
   end Big_Number_Mod_Utils_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   X := Get_Random(X_3812);
	   N := Get_Random(X_3812);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_3812);
	   N := Get_Random(X_3812);
	   Assert(X /= N, "Not Random");
   
   end Big_Number_Mod_Utils_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
	   X := Get_Random(X_2048);
	   N := Get_Random(X_2048);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_2048);
	   N := Get_Random(X_2048);
	   Assert(X /= N, "Not Random");
   
   end Big_Number_Mod_Utils_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
	   X := Get_Random(X_1025);
	   N := Get_Random(X_1025);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_1025);
	   N := Get_Random(X_1025);
	   Assert(X /= N, "Not Random");
   
   end Big_Number_Mod_Utils_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
	   X := Get_Random(X_1025);
	   N := Get_Random(X_1025);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_1025);
	   N := Get_Random(X_1025);
	   Assert(X /= N, "Not Random");
   
   end Big_Number_Mod_Utils_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
	   X := Get_Random(X_1024);
	   N := Get_Random(X_1024);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_1024);
	   N := Get_Random(X_1024);
	   Assert(X /= N, "Not Random");
   
	   
   end Big_Number_Mod_Utils_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

	   X := Get_Random(X_768);
	   N := Get_Random(X_768);
	   Assert(X /= N, "Not Random");
   
	   X := Get_Random(X_768);
	   N := Get_Random(X_768);
	   Assert(X /= N, "Not Random");

   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_Rand_Mod_Utils;
