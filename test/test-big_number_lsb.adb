with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_LSB is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;

	use Big_Number_Constants;
    
	X_4096, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0, P, Q, R, Z : 
	Big_Unsigned;

------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big Number Tests  ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin

		Register_Routine(T, Big_Number_Test1'Access,"Set least significant Bit");

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
   	   P := Big_Unsigned_Last;
	   Set_Least_Significant_Bit(P);
	   Assert(P = Big_Unsigned_Last, "Failed with Big_Unsigned_Last Bit.");

	   P := 2 * P;
	   Set_Least_Significant_Bit(P);
	   Assert(P = Big_Unsigned_Last, "Failed with 4096 Bit.");
	   
	   P := X_1024;
	   Set_Least_Significant_Bit(P);
	   Assert(P = X_1024, "Failed with 1024 Bit.");
	   
	   P := X_1025;
	   Set_Least_Significant_Bit(P);
	   Q := X_1025 + 1;
	   Assert(P = Q, "Failed with 1025 Bit.");
	   
	   P := X_768;
	   Set_Least_Significant_Bit(P);
	   Q := X_768 + 1;
	   Assert(P = Q, "Failed with 768 Bit.");
	   
	   P := X_1;
	   Set_Least_Significant_Bit(P);
	   Assert(P = X_1, "Failed with 1 Bit.");
	   
	   P := X_0;
	   Set_Least_Significant_Bit(P);
	   Assert(P = X_1, "Failed with 0 Bit.");
	   
   end Big_Number_Test1;

------------------------------------------------------------------------------------

end Test.Big_Number_LSB;
