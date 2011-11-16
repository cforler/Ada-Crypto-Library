with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;
with Big_Number_Div_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Div is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
	
	use Big_Number_Constants;
	use Big_Number_Div_Results;
--	use Ada.Text_IO;
    
    X_4096, X_3812, X_1025, X_1024, X_768, X_1, X_0: Big_Unsigned;
	X, Result: Big_Unsigned;
	
------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
		
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number_Div Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Constants;
		Register_Routine(T, Big_Number_Div_Test1'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test2'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test3'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test4'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test5'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test6'Access,"Simple Division");
		Register_Routine(T, Big_Number_Div_Test7'Access,"Simple Division");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number_Div Test ------------------------------
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

   procedure Big_Number_Div_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

  	   X := X_4096/X_4096;
   	   Assert(X = X_1 and X = 1, "Failed with 4096 Bit.");
   	   
   	   X := X_4096/X_3812;
   	   Result := To_Big_Unsigned("52746481033896654832755311750735295224685184117" &
   	   "649504282494752424678917934021778020556");
   	   Assert(X = Result, "Failed with 4096 and 3812 Bit.");
   	   
   	   X := X_4096/X_1025;
	   Result := To_Big_Unsigned(Result_0);
   	   Assert(X = Result, "Failed with 4096 and 1025 Bit");
   	   
   	   X := X_4096/X_1024;
	   Result := To_Big_Unsigned(Result_1);
   	   Assert(X = Result, "Failed with 4096 and 1024 Bit");

   	   X := X_4096/X_768;
	   Result := To_Big_Unsigned(Result_2);
   	   Assert(X = Result, "Failed with 4096 and 768 Bit.");
   	   
   	   X := X_4096/X_1;
   	   Assert(X = X_4096, "Failed with 4096 and 1 Bit.");
   	   --X := X_4096/X_0;
   	   --Assert(X = 0, "Failed with 4096 Bit.");

   end Big_Number_Div_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
  	   X := X_3812/X_4096;
   	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");
   	   
   	   X := X_3812/X_3812;
   	   Assert(X = X_1, "Failed with 3812 Bit.");
   	   
   	   X := X_3812/X_1025;
	   Result := To_Big_Unsigned(Result_3);
   	   Assert(X = Result, "Failed with 3812 and 1025 Bit.");
   	   
   	   X := X_3812/X_1024;
	   Result := To_Big_Unsigned(Result_4);
   	   Assert(X = Result, "Failed with 3812 and 1024 Bit.");

   	   X := X_3812/X_768;
	   Result := To_Big_Unsigned(Result_5);
   	   Assert(X = Result, "Failed with 3812 and 768 Bit.");
   	   
   	   X := X_3812/X_1;
   	   Assert(X = X_3812, "Failed with 3812 and 1 Bit.");
   	   
   	   --X := X_4096/X_0;
   	   --Assert(X = ZERO_ERROR, "Failed with 3812 and 0 Bit.");

   end Big_Number_Div_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1025/X_4096;
   	   Assert(X = X_0, "Failed with 1025 Bit.");
   	   X := X_1025/X_3812;
   	   Assert(X = X_0, "Failed with 1025 Bit.");
   	   
   	   X := X_1025/X_1025;
   	   Assert(X = X_1, "Failed with 1025 Bit.");
   	   
   	   X := X_1025/X_1024;
   	   Assert(X = X_1, "Failed with 1025 and 1024 Bit.");

   	   X := X_1025/X_768;
	   Result := To_Big_Unsigned("17079926207563796711439765047027386489559536728" &
	   "9984996551649399560356639804054");
   	   Assert(X = Result, "Failed with 1025 and 768 Bit.");

   	   X := X_1025/X_1;
   	   Assert(X = X_1025, "Failed with 1025 and 1024 Bit.");

   	   --X := X_1025/X_0;
   	   --Assert(X = 0, "Failed with 1025 and 1024 Bit.");
   	   
   end Big_Number_Div_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1024/X_4096;
   	   Assert(X = X_0, "Failed with 1024 Bit.");
   	   X := X_1024/X_3812;
   	   Assert(X = X_0, "Failed with 1024 Bit.");
   	   X := X_1024/X_1025;
   	   Assert(X = X_0, "Failed with 1024 Bit.");
   	   
   	   X := X_1024/X_1024;
   	   Assert(X = X_1, "Failed with 1024 and 1024 Bit.");

   	   X := X_1024/X_768;
	   Result := To_Big_Unsigned("17079926207563796711439765047027386489559536728" &
	   "9984996551649399560356639804054");
   	   Assert(X = Result, "Failed with 1024 and 768 Bit.");

   	   X := X_1024/X_1;
   	   Assert(X = X_1024, "Failed with 1024 and 1 Bit.");

   	   --X := X_1024/X_0;
   	   --Assert(X = 0, "Failed with 1024 Bit.");
   	   
   	   
   end Big_Number_Div_Test4;

--------------------------------------------------------------------------------------
---------------------------------------- Test 5 --------------------------------------
--------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := X_768/X_768;
   	   Assert(X = X_1, "Failed with 768 Bit.");

   	   X := X_768/X_1;
   	   Assert(X = X_768, "Failed with 768 Bit.");

   	   --X := X_768/X_0;
   	   --Assert(X = 0, "Failed with 768 Bit.");

   end Big_Number_Div_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := X_1/X_1;
   	   Assert(X = X_1 and X = 1, "Failed with 1 Bit.");

   	   --X := X_1/X_0;
   	   --Assert(X = X_1 and X = 0, "Failed with 1 Bit.");

   end Big_Number_Div_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Div_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   --X := X_0/X_0;
   	   --Assert(X = X_0 and X = 0, "Failed with 0 Bit.");
   	   
   	   X := X_0/X_1;
   	   Assert(X = X_0 and X = 0, "Failed with 1 Bit.");

   end Big_Number_Div_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Div;
