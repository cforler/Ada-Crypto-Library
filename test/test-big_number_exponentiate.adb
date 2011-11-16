with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Number_Exponentiate_Results; 
--with Ada.Text_IO; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Exponentiate is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
	
--	use Ada.Text_IO;
    use Big_Number_Constants; 
	use Big_Number_Exponentiate_Results; 

    X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0: Big_Unsigned;
	A, B, X, Result: Big_Unsigned;
	
------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin

		X_2048 := To_Big_Unsigned(Cons_2048);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_582  := To_Big_Unsigned(Cons_582);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
-------------------- Register Big Number Exponentiate Tests ------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Exponentiate_Test1'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test2'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test3'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test4'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test5'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test6'Access,"Exponentiate");
		Register_Routine(T, Big_Number_Exponentiate_Test7'Access,"Exponentiate");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number_Exponentiate Test ------------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number Tests");
	end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Constants;
	   A := To_Big_Unsigned("4");
   	   X := X_1024 ** A;
	   Result := To_Big_Unsigned(Result_0);
   	   Assert(X = Result, "Failed with 1024 Bit and 4 as exponent.");
	   X := X_0**A;
	   Assert(X = X_0, "Failed with 0 Bit and 4 as exponent.");
	   X := X_1**A;
	   Assert(X = X_1, "Failed with 1 Bit and 4 as exponant.");

	   A := To_Big_Unsigned("2");
   	   X := X_2048 ** A;
	   Result := To_Big_Unsigned(Result_1);
   	   Assert(X = Result, "Failed with square of 2048 Bit.");
   
   end Big_Number_Exponentiate_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
	   A := To_Big_Unsigned("34");
	   B := To_Big_Unsigned("907033160381558231474246103786079923");
   	   X := B ** A;
	   Result := To_Big_Unsigned(Result_2);
   	   Assert(X = Result, "Failed with 120 Bit and 34 as exponent.");

   end Big_Number_Exponentiate_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
	   A := To_Big_Unsigned("7");
   	   X := X_582 ** A;
	   Result := To_Big_Unsigned(Result_3);
   	   Assert(X = Result, "Failed with 582 Bit and 7 as exponent.");
   	   
   end Big_Number_Exponentiate_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
	   A := To_Big_Unsigned("6");
   	   X := X_768 ** A;
	   Result := To_Big_Unsigned(Result_4);
   	   Assert(X = Result, "Failed with 768 Bit and 6 as exponent.");

   end Big_Number_Exponentiate_Test4;

--------------------------------------------------------------------------------------
---------------------------------------- Test 5 --------------------------------------
--------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   A := To_Big_Unsigned("5");
   	   X := X_768 ** A;
	   Result := To_Big_Unsigned(Result_5);
   	   Assert(X = Result, "Failed with 768 Bit and 5 as exponent.");
	   
   end Big_Number_Exponentiate_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
	   A := To_Big_Unsigned("3");
   	   X := X_1025 ** A;
	   Result := To_Big_Unsigned(Result_6);
   	   Assert(X = Result, "Failed with 1025 Bit and 3 as exponent.");
	   
   end Big_Number_Exponentiate_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Exponentiate_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := X_768 ** X_0;
   	   Assert(X = X_1, "Failed with 768 Bit and 0 as exponent.");
   	   
   	   X := X_768 ** X_1;
   	   Assert(X = X_768, "Failed with 768 Bit and 1 as exponent.");

   	   X := X_582 ** X_1;
   	   Assert(X = X_582, "Failed with 582 Bit and 1 as exponent.");
   	   
   	   X := X_582 ** X_0;
   	   Assert(X = X_1, "Failed with 582 Bit and 0 as exponent.");

   end Big_Number_Exponentiate_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Exponentiate;
