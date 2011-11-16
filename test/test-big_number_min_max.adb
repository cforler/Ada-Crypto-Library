with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Min_Max is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;

	use Big_Number_Constants;	
    
    P, X_4096, X_4095, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0: 
    Big_Unsigned;
	
------------------------------------------------------------------------------------
----------------------------------- Constants --------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
		
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_2048 := To_Big_Unsigned(Cons_2048);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768 := To_Big_Unsigned(Cons_768);
		X_582 := To_Big_Unsigned(Cons_582);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
------------------------- Register Big Number Min/Max Tests ------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Min'Access,"Minimum");
		Register_Routine(T, Big_Number_Max'Access,"Maximum");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number3 Test ------------------------------
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

   procedure Big_Number_Min(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	
   	   Constants;

	   P := Min(X_4095, X_4096);
	   Assert(P = X_4095, "Failed with 4095 and 4096.");
   	   
	   P := Min(X_4096, X_4095);
	   Assert(P = X_4095, "Failed with 4096 and 4095.");
   	   
	   P := Min(X_3812, X_4096);
	   Assert(P = X_3812, "Failed with 3812 and 4096.");
   	   
	   P := Min(X_4096, X_3812);
	   Assert(P = X_3812, "Failed 4096 and 3812.");
   	   
	   P := Min(X_2048, X_4096);
	   Assert(P = X_2048, "Failed with 2048 and 4096.");
   	   
	   P := Min(X_4096, X_768);
	   Assert(P = X_768, "Failed with 4096 and 768.");
   	   
	   P := Min(X_582, X_4096);
	   Assert(P = X_582, "Failed with 582 and 4096.");
   	   
	   P := Min(X_4096, X_582);
	   Assert(P = X_582, "Failed with 4096 and 582.");
   	   
	   P := Min(X_1, X_4096);
	   Assert(P = X_1, "Failed with 1 and 4096.");
   	   
	   P := Min(X_4096, X_1);
	   Assert(P = X_1, "Failed with 4096 and 1.");
   	   
	   P := Min(X_0, X_4096);
	   Assert(P = X_0, "Failed with 0 and 4096.");
   	   
	   P := Min(X_4096, X_0);
	   Assert(P = X_0, "Failed with 4096 and 0.");
   	   
	   P := Min(X_4095, X_2048);
	   Assert(P = X_2048, "Failed with 4095 and 2048.");
   	   
	   P := Min(X_1025, X_4095);
	   Assert(P = X_1025, "Failed with 1025 and 4095.");
   	   
	   P := Min(X_3812, X_2048);
	   Assert(P = X_2048, "Failed with 3812 and 2048.");
   	   
	   P := Min(X_1025, X_3812);
	   Assert(P = X_1025, "Failed with 1025 and 3812.");
   	   
	   P := Min(X_768, X_2048);
	   Assert(P = X_768, "Failed with 768 and 2048.");
   	   
	   P := Min(X_768, X_3812);
	   Assert(P = X_768, "Failed with 768 and 3812.");
   	   
	   P := Min(X_1025, X_1024);
	   Assert(P = X_1024, "Failed with 1025 and 1024.");
   	   
   end Big_Number_Min;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Max(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   P := Max(X_4095, X_4096);
	   Assert(P = X_4096, "Failed with 4095 and 4096 Bit.");
   	   
	   P := Max(X_4096, X_4095);
	   Assert(P = X_4096, "Failed with 4096 and 4095 Bit.");
   	   
	   P := Max(X_3812, X_4096);
	   Assert(P = X_4096, "Failed with 3812 and 4096.");
   	   
	   P := Max(X_4096, X_3812);
	   Assert(P = X_4096, "Failed with 4096 and 3812.");
   	   
	   P := Max(X_2048, X_4096);
	   Assert(P = X_4096, "Failed with 2048 and 4096.");
   	   
	   P := Max(X_4095, X_2048);
	   Assert(P = X_4095, "Failed with 4095 and 2048.");
   	   
	   P := Max(X_1025, X_3812);
	   Assert(P = X_3812, "Failed with 1025 and 3812.");
   	   
	   P := Max(X_1024, X_1025);
	   Assert(P = X_1025, "Failed with 1024 and 1025.");
   	   
	   P := Max(X_1025, X_1024);
	   Assert(P = X_1025, "Failed with 1024 and 1025.");
   	   
	   P := Max(X_1024, X_1024);
	   Assert(P = X_1024, "Failed with 1024.");
   	   
	   P := Max(X_768, X_582);
	   Assert(P = X_768, "Failed with 768 and 582.");
   	   
	   P := Max(X_1025, X_768);
	   Assert(P = X_1025, "Failed with 1025 and 768.");
   	   
	   P := Max(X_582, X_768);
	   Assert(P = X_768, "Failed with 582 and 768.");
   	   
	   P := Max(X_1, X_582);
	   Assert(P = X_582, "Failed with 1 and 582.");
   	   
	   P := Max(X_0, X_1);
	   Assert(P = X_1, "Failed with 0 and 1.");
   	   
	   P := Max(X_1, X_0);
	   Assert(P = X_1, "Failed with 1 and 0.");
   	   
	   P := Max(X_4096, X_0);
	   Assert(P = X_4096, "Failed with 4096 and 0.");
   	   
   end Big_Number_Max;
	   
------------------------------------------------------------------------------------

end Test.Big_Number_Min_Max;
