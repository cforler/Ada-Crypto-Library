with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Inc is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

   package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    
    use Big_Number_Constants;
	
	X_1025, X_1024, X_768, X_1, X_0, Q  : Big_Unsigned;

------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
	   X_1025 := To_Big_Unsigned(Cons_1025);
	   X_1024 := To_Big_Unsigned(Cons_1024);
	   X_768  := To_Big_Unsigned(Cons_768);
	   X_1 := To_Big_Unsigned("1");
	   X_0 := To_Big_Unsigned("0");
	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number Test 1 ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin

		Register_Routine(T, Big_Number_Test1'Access,"Increase with 4096 Bit");
		Register_Routine(T, Big_Number_Test2'Access,"Increase with 1024 and 1025 Bit");
		Register_Routine(T, Big_Number_Test3'Access,"Increase with 768 Bit");
		Register_Routine(T, Big_Number_Test4'Access,"Increase with 1 Bit");
		Register_Routine(T, Big_Number_Test5'Access,"Increase with 0 Bit");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big Number Tests ------------------------------
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

   procedure Big_Number_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	 
   	   Constants;
   	   Q := Big_Unsigned_Last;
   	   Inc(Q); 
	   Assert(Q = 0, "Failed.");

   end Big_Number_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Q := X_1024;
   	   Inc(Q); 
	   Assert(Q = X_1025, "Failed.");

   end Big_Number_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
   	   Q := X_768;
   	   Inc(Q); 
	   Assert(Q = X_768 + 1, "Failed.");

   end Big_Number_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Q := X_1;
   	   Inc(Q); 
	   Assert(Q = 2, "Failed.");

   end Big_Number_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Q := X_0;
   	   Inc(Q); 
	   Assert(Q = X_1, "Failed.");

   end Big_Number_Test5;

------------------------------------------------------------------------------------

end Test.Big_Number_Inc;
