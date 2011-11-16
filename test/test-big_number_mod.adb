with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Number_Mod_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Mod is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
	
	use Big_Number_Mod_Results;
	use Big_Number_Constants;

    X_4096, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0: Big_Unsigned;
	X, Result: Big_Unsigned;
	
------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_2048 := To_Big_Unsigned(Cons_2048);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_582  := To_Big_Unsigned(Cons_582);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");
		
	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number_Mod Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Test1'Access,"Modulo Operation 1");
		Register_Routine(T, Big_Number_Mod_Test2'Access,"Modulo Operation 2");
		Register_Routine(T, Big_Number_Mod_Test3'Access,"Modulo Operation 3");
		Register_Routine(T, Big_Number_Mod_Test4'Access,"Modulo Operation 4");
		Register_Routine(T, Big_Number_Mod_Test5'Access,"Modulo Operation 5");
		Register_Routine(T, Big_Number_Mod_Test6'Access,"Modulo Operation 6");
		Register_Routine(T, Big_Number_Mod_Test7'Access,"Modulo Operation 7");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number_Mod Test ------------------------------
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

   procedure Big_Number_Mod_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   Constants;

   	   X := X_4096 mod X_4096;
   	   Assert(X = X_0, "Failed with Big Number 4096 Bit.");
   	   
   	   X := X_4096 mod X_3812;
	   Result := To_Big_Unsigned(Cons_1);
   	   Assert(X = Result, "Failed with Big Number 4096 and 3812 Bit.");

   	   X := X_4096 mod X_2048;
   	   Assert(X = X_0, "Failed with Big Number 4096 and 2048 Bit.");
   	   
   	   X := X_4096 mod X_1025;
   	   Assert(X = X_1024, "Failed with Big Number 4096 and 1025 Bit.");

   	   X := X_4096 mod X_1024;
   	   Assert(X = X_0, "Failed with Big Number 4096 and 1024 Bit.");
   	   
   	   X := X_4096 mod X_768;
	   Result := To_Big_Unsigned(Cons_2);
   	   Assert(X = Result, "Failed with Big Number 4096 and 768 Bit.");

	   X := X_4096 mod X_582;
	   Result := To_Big_Unsigned(Cons_3);
   	   Assert(X = Result, "Failed with Big Number 4096 and 582 Bit.");

	   X := X_4096 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 4096 and 1 Bit.");
	   
   end Big_Number_Mod_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_3812 mod X_4096;
   	   Assert(X = X_3812, "Failed with Big Number 3812 and 4096 Bit.");
  	   
   	   X := X_3812 mod X_3812;
   	   Assert(X = X_0, "Failed with Big Number 3812 Bit.");
   	   
   	   X := X_3812 mod X_2048;
	   Result := To_Big_Unsigned(Cons_4);
   	   Assert(X = Result, "Failed with Big Number 3812 and 2048 Bit.");

   	   X := X_3812 mod X_1025;
	   Result := To_Big_Unsigned(Cons_5);
   	   Assert(X = Result, "Failed with Big Number 3812 and 1025 Bit.");

   	   X := X_3812 mod X_1024;
	   Result := To_Big_Unsigned(Cons_6);
   	   Assert(X = Result, "Failed with Big Number 3812 and 1024 Bit.");
   	   
   	   X := X_3812 mod X_768;
	   Result := To_Big_Unsigned(Cons_7);
   	   Assert(X = Result, "Failed with Big Number 3812 and 768 Bit.");
   	   
   	   X := X_3812 mod X_582;
	   Result := To_Big_Unsigned(Cons_8);
   	   Assert(X = Result, "Failed with Big Number 3812 and 582 Bit.");
   	   
   	   X := X_3812 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 3812 and 1 Bit.");

   end Big_Number_Mod_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
   	   X := X_2048 mod X_4096;
   	   Assert(X = X_2048, "Failed with Big Number 2048 and 4096 Bit.");
   	   
   	   X := X_2048 mod X_3812;
   	   Assert(X = X_2048, "Failed with Big Number 2048 and 3812 Bit.");
   	   
   	   X := X_2048 mod X_2048;
   	   Assert(X = X_0, "Failed with Big Number 2048 Bit.");
   	   
   	   X := X_2048 mod X_1025;
   	   Assert(X = X_1024, "Failed with Big Number 2048 and 1025 Bit.");

   	   X := X_2048 mod X_1024;
   	   Assert(X = X_0, "Failed with Big Number 2048 and 1024 Bit.");

   	   X := X_2048 mod X_768;
	   Result := To_Big_Unsigned(Cons_9);
   	   Assert(X = Result, "Failed with Big Number 2048 and 768 Bit.");

   	   X := X_2048 mod X_582;
	   Result := To_Big_Unsigned(Cons_10);
   	   Assert(X = Result, "Failed with Big Number 2048 and 582 Bit.");

   	   X := X_2048 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 2048 and 1 Bit.");

   end Big_Number_Mod_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1025 mod X_4096;
   	   Assert(X = X_1025, "Failed with Big Number 1025 and 4096 Bit.");
   	   
   	   X := X_1025 mod X_3812;
   	   Assert(X = X_1025, "Failed with Big Number 1025 and 3812 Bit.");
   	   
   	   X := X_1025 mod X_2048;
   	   Assert(X = X_1025, "Failed with Big Number 1025 and 2048 Bit.");
   	   
   	   X := X_1025 mod X_1025;
   	   Assert(X = X_0, "Failed with Big Number 1025 Bit.");

   	   X := X_1025 mod X_1024;
   	   Assert(X = X_1, "Failed with Big Number 1025 and 1024 Bit.");

   	   X := X_1025 mod X_768;
	   Result := To_Big_Unsigned(Cons_11);
   	   Assert(X = Result, "Failed with Big Number 1025 and 768 Bit.");

   	   X := X_1025 mod X_582;
	   Result := To_Big_Unsigned(Cons_12);
   	   Assert(X = Result, "Failed with Big Number 1025 and 582 Bit.");

   	   X := X_1025 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 1025 and 1 Bit.");

   end Big_Number_Mod_Test4;

--------------------------------------------------------------------------------------
---------------------------------------- Test 5 --------------------------------------
--------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1024 mod X_4096;
   	   Assert(X = X_1024, "Failed with Big Number 1024 and 4096 Bit.");
   	   
   	   X := X_1024 mod X_3812;
   	   Assert(X = X_1024, "Failed with Big Number 1024 and 3812 Bit.");
   	   
   	   X := X_1024 mod X_2048;
   	   Assert(X = X_1024, "Failed with Big Number 1024 and 2048 Bit.");
   	   
   	   X := X_1024 mod X_1025;
   	   Assert(X = X_1024, "Failed with Big Number 1024 and 1025 Bit.");

   	   X := X_1024 mod X_1024;
   	   Assert(X = X_0, "Failed with Big Number 1024 Bit.");

   	   X := X_1024 mod X_768;
	   Result := To_Big_Unsigned(Cons_11);
	   Result := Result - 1;
   	   Assert(X = Result, "Failed with Big Number 1024 and 768 Bit.");

   	   X := X_1024 mod X_582;
	   Result := To_Big_Unsigned(Cons_12);
	   Result := Result - 1;
   	   Assert(X = Result, "Failed with Big Number 1024 and 582 Bit.");

   	   X := X_1024 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 1024 and 1 Bit.");

   end Big_Number_Mod_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := X_768 mod X_4096;
   	   Assert(X = X_768, "Failed with Big Number 768 and 4096 Bit.");
   	   
   	   X := X_768 mod X_3812;
   	   Assert(X = X_768, "Failed with Big Number 768 and 3812 Bit.");
   	   
   	   X := X_768 mod X_2048;
   	   Assert(X = X_768, "Failed with Big Number 768 and 2048 Bit.");
   	   
   	   X := X_768 mod X_1025;
   	   Assert(X = X_768, "Failed with Big Number 768 and 1025 Bit.");

   	   X := X_768 mod X_1024;
   	   Assert(X = X_768, "Failed with Big Number 768 and 1024 Bit.");

   	   X := X_768 mod X_768;
   	   Assert(X = X_0, "Failed with Big Number 768 Bit.");

   	   X := X_768 mod X_582;
	   Result := To_Big_Unsigned(Cons_13);
   	   Assert(X = Result, "Failed with Big Number 768 and 582 Bit.");

   	   X := X_768 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 768 and 1 Bit.");
	   
   end Big_Number_Mod_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := X_582 mod X_4096;
   	   Assert(X = X_582, "Failed with Big Number 582 and 4096 Bit.");
   	   
   	   X := X_582 mod X_3812;
   	   Assert(X = X_582, "Failed with Big Number 582 and 3812 Bit.");
   	   
   	   X := X_582 mod X_2048;
   	   Assert(X = X_582, "Failed with Big Number 582 and 2048 Bit.");
   	   
   	   X := X_582 mod X_1025;
   	   Assert(X = X_582, "Failed with Big Number 582 and 1025 Bit.");

   	   X := X_582 mod X_1024;
   	   Assert(X = X_582, "Failed with Big Number 582 and 1024 Bit.");

   	   X := X_582 mod X_768;
   	   Assert(X = X_582, "Failed with Big Number 582 and 768 Bit.");

   	   X := X_582 mod X_582;
   	   Assert(X = X_0, "Failed with Big Number 582 Bit.");

   	   X := X_582 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 582 and 1 Bit.");
   	   
   	   X := X_1 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 1 Bit.");
	   
   	   X := X_1 mod X_4096;
   	   Assert(X = X_1, "Failed with Big Number 1 and 4096 Bit.");
	   
   	   X := X_0 mod X_4096;
   	   Assert(X = X_0, "Failed with Big Number 0 and 4096 Bit.");
	   
   	   X := X_0 mod X_1;
   	   Assert(X = X_0, "Failed with Big Number 0 and 1 Bit.");
	   
   end Big_Number_Mod_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Mod;
