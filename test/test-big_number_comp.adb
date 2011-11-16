with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Comp is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;

	use Big_Number_Constants;	
    
   X_4096, X_4095, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_16, X_10, X_5, 
   X_4, X_3, X_2, X_1, X_0: Big_Unsigned;
	
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
		X_16 := To_Big_Unsigned("16");
		X_10 := To_Big_Unsigned("10");
	   	X_5 := To_Big_Unsigned("32");
		X_4 := To_Big_Unsigned("4");
		X_3 := To_Big_Unsigned("3");
		X_2 := To_Big_Unsigned("2");
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Comparison Tests -----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Comp_Test1'Access,"Simple comparison");
		Register_Routine(T, Big_Number_Comp_Test2'Access,"Simple smaller than comparison");
		Register_Routine(T, Big_Number_Comp_Test3'Access,"Simple bigger than comparison");
		Register_Routine(T, Big_Number_Comp_Test4'Access,"Simple comparison smaller or equal than");
		Register_Routine(T, Big_Number_Comp_Test5'Access,"Simple comparison bigger or equal than");

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

   procedure Big_Number_Comp_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	
   	   Constants;
   	   
   	   Assert(X_4096 = Big_Unsigned_Last, "Failed with Big_Unsigned_Last.");
   	   Assert(Big_Unsigned_Last = X_4096, "Failed with Big_Unsigned_Last.");
   	   Assert(Big_Unsigned_Zero = X_0, "Failed with Big_Unsigned_Zero.");
   	   Assert(X_0 = Big_Unsigned_Zero, "Failed with Big_Unsigned_Zero.");
   	   Assert(X_1 = Big_Unsigned_One, "Failed with Big_Unsigned_One.");
   	   Assert(Big_Unsigned_One = X_1, "Failed with Big_Unsigned_One.");
   	   Assert(X_2 = Big_Unsigned_Two, "Failed with Big_Unsigned_Two.");
   	   Assert(Big_Unsigned_Two = X_2, "Failed with Big_Unsigned_Two.");
   	   Assert(X_3 = Big_Unsigned_Three, "Failed with Big_Unsigned_Three.");
   	   Assert(Big_Unsigned_Three = X_3, "Failed with Big_Unsigned_Three.");
   	   Assert(X_4 = Big_Unsigned_Four, "Failed with Big_Unsigned_Four");
   	   Assert(Big_Unsigned_Four = X_4, "Failed with Big_Unsigned_Four.");
   	   Assert(X_10 = Big_Unsigned_Ten, "Failed with Big_Unsigned_Ten.");
   	   Assert(Big_Unsigned_Ten = X_10, "Failed with Big_Unsigned_Ten.");
   	   Assert(X_16 = Big_Unsigned_Sixteen, "Failed with Big_Unsigned_Sixteen.");
   	   Assert(Big_Unsigned_Sixteen = X_16, "Failed with Big_Unsigned_Sixteen.");
   	   Assert(Big_Unsigned_Zero = 0, "Big_Unsigned_Zero failed.");
   	   Assert(Big_Unsigned_One = 1, "Big_Unsigned_One failed.");
   	   Assert(Big_Unsigned_Two = 2, "Big_Unsigned_Two failed.");
   	   Assert(Big_Unsigned_Three = 3, "Big_Unsigned_Three failed.");
   	   Assert(Big_Unsigned_Four = 4, "Big_Unsigned_Four failed.");
   	   Assert(Big_Unsigned_Ten = 10, "Big_Unsigned_Ten failed.");
   	   Assert(Big_Unsigned_Sixteen = 16, "Big_Unsigned_Sixteen failed.");
   	   Assert(Big_Unsigned_First = 0 , "Big_Unsigned_First failed.");
   	   Assert(Big_Unsigned_Last = X_4096, "Big_Unsigned_Last failed.");
	   Assert(X_0 = 0, "Comparison with 0.");
	   Assert(0 = X_0, "Comparison with 0.");
	   Assert(X_1 = 1, "Comparison with 1.");
	   Assert(1 = X_1, "Comparison with 1.");

   end Big_Number_Comp_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Comp_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Assert(X_4095 < X_4096, "Failed with 4095 and 4096 Bit.");
   	   Assert(0 < X_1, "Failed with 0 and 1 Bit.");
   	   Assert(X_0 < X_1, "Failed with 0 and 1 Bit.");
   	   Assert(NOT (0 < X_0), "Comparison smaller than.");
   	   Assert(X_1 < X_5, "Failed with 1 und 5 Bit.");
   	   Assert(X_4 < X_5, "Failed with 2 und 5 Bit.");
   	   Assert(Big_Unsigned_One < X_5, "Failed with 1 und 5 Bit.");
   	   Assert(Big_Unsigned_One < Big_Unsigned_Two, "Failed with 1 und 2 Bit.");
   	   Assert(Big_Unsigned_Two < Big_Unsigned_Three, "Failed with 2 und 2 Bit.");
   	   Assert(Big_Unsigned_Three < Big_Unsigned_Four, "Failed with 2 und 3 Bit.");
   	   Assert(Big_Unsigned_Four < Big_Unsigned_Ten, "Failed with 2 und 4 Bit.");
   	   Assert(Big_Unsigned_Ten < Big_Unsigned_Sixteen, "Failed with 4 und 5 Bit.");
   	   Assert(Big_Unsigned_Sixteen < Big_Unsigned_Last, "Failed with 5 und 4096 Bit.");
   	   Assert(Big_Unsigned_Two < X_5, "Failed with 2 und 5 Bit.");
   	   Assert(Big_Unsigned_Three < X_5, "Failed with 2 und 5 Bit.");
   	   Assert(Big_Unsigned_Four < X_5, "Failed with 3 und 5 Bit.");
   	   Assert(X_4095 < X_4096, "Failed with 4095 and 4096 Bit.");
   	   Assert(X_3812 < X_4096, "Failed with 3812 and 4096 Bit.");
   	   Assert(X_2048 < X_4096, "Failed with 2048 and 4096 Bit.");
   	   Assert(X_1025 < X_4096, "Failed with 1025 and 4096 Bit.");
   	   Assert(X_1024 < X_4096, "Failed with 1024 and 4096 Bit.");
   	   Assert(X_768 < X_4096, "Failed with 768 and 4096 Bit.");
   	   Assert(X_582 < X_4096, "Failed with 582 and 4096 Bit.");
   	   Assert(X_1 < X_4096, "Failed with 1 and 4096 Bit.");
   	   Assert(X_0 < X_4096, "Failed with 0 and 4096 Bit.");
   	   Assert(X_2048 < X_3812, "Failed with 2048 and 3812 Bit.");
   	   Assert(X_1025 < X_3812, "Failed with 1025 and 3812 Bit.");
   	   Assert(X_1024 < X_3812, "Failed with 1024 and 3812 Bit.");
   	   Assert(X_768 < X_3812, "Failed with 768 and 3812 Bit.");
   	   Assert(X_582 < X_3812, "Failed with 582 and 3812 Bit.");
   	   Assert(X_1 < X_3812, "Failed with 1 and 3812 Bit.");
   	   Assert(X_0 < X_3812, "Failed with 0 and 3812 Bit.");
   	   Assert(X_1025 < X_2048, "Failed with 1025 and 2048 Bit.");
   	   Assert(X_1024 < X_2048, "Failed with 1024 and 2048 Bit.");
   	   Assert(X_768 < X_2048, "Failed with 768 and 2048 Bit.");
   	   Assert(X_582 < X_2048, "Failed with 582 and 2048 Bit.");
   	   Assert(X_1 < X_2048, "Failed with 1 and 2048 Bit.");
   	   Assert(X_0 < X_2048, "Failed with 0 and 2048 Bit.");
   	   Assert(X_1024 < X_1025, "Failed with 1024 and 1025 Bit.");
   	   Assert(X_768 < X_1025, "Failed with 768 and 1025 Bit.");
   	   Assert(X_582 < X_1025, "Failed with 582 and 1025 Bit.");
   	   Assert(X_1 < X_1025, "Failed with 1 and 1025 Bit.");
   	   Assert(X_0 < X_1025, "Failed with 0 and 1025 Bit.");
   	   Assert(X_768 < X_1024, "Failed with 768 and 1024 Bit.");
   	   Assert(X_582 < X_1024, "Failed with 582 and 1024 Bit.");
   	   Assert(X_1 < X_1024, "Failed with 1 and 1024 Bit.");
   	   Assert(X_0 < X_1024, "Failed with 0 and 1024 Bit.");
   	   Assert(X_582 < X_768, "Failed with 582 and 768 Bit.");
   	   Assert(X_1 < X_768, "Failed with 1 and 768 Bit.");
   	   Assert(X_0 < X_768, "Failed with 0 and 768 Bit.");
   	   Assert(X_1 < X_582, "Failed with 1 and 582 Bit.");
   	   Assert(X_0 < X_582, "Failed with 0 and 582 Bit.");

   end Big_Number_Comp_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Comp_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Assert(X_4096 > X_4095, "Failed with 4096 and 4095 Bit.");
   	   Assert(X_4096 > X_3812, "Failed with 4096 and 3812 Bit.");
   	   Assert(X_4096 > X_2048, "Failed with 4096 and 2048 Bit.");
   	   Assert(X_4096 > X_1025, "Failed with 4096 and 1025 Bit.");
   	   Assert(X_4096 > X_1024, "Failed with 4096 and 1024 Bit.");
   	   Assert(X_4096 > X_768, "Failed with 4096 and 768 Bit.");
   	   Assert(X_4096 > X_582, "Failed with 4096 and 582 Bit.");
   	   Assert(X_4096 > X_1, "Failed with 4096 and 1 Bit.");
   	   Assert(X_4096 > X_0, "Failed with 4096 and 0 Bit.");
   	   Assert(X_4095 > X_3812, "Failed with 4095 and 3812 Bit.");
   	   Assert(X_4095 > X_2048, "Failed with 4095 and 2048 Bit.");
   	   Assert(X_4095 > X_1025, "Failed with 4095 and 1025 Bit.");
   	   Assert(X_4095 > X_1024, "Failed with 4095 and 1024 Bit.");
   	   Assert(X_4095 > X_768, "Failed with 4095 and 768 Bit.");
   	   Assert(X_4095 > X_582, "Failed with 4095 and 582 Bit.");
   	   Assert(X_4095 > X_1, "Failed with 4095 and 1 Bit.");
   	   Assert(X_4095 > X_0, "Failed with 4095 and 0 Bit.");
   	   Assert(X_3812 > X_2048, "Failed with 3812 and 2048 Bit.");
   	   Assert(X_3812 > X_1025, "Failed with 3812 and 1025 Bit.");
   	   Assert(X_3812 > X_1024, "Failed with 3812 and 1024 Bit.");
   	   Assert(X_3812 > X_768, "Failed with 3812 and 768 Bit.");
   	   Assert(X_3812 > X_582, "Failed with 3812 and 582 Bit.");
   	   Assert(X_3812 > X_1, "Failed with 3812 and 1 Bit.");
   	   Assert(X_3812 > X_0, "Failed with 3812 and 0 Bit.");
   	   Assert(X_2048 > X_1025, "Failed with 2048 and 1025 Bit.");
   	   Assert(X_2048 > X_1024, "Failed with 2048 and 1024 Bit.");
   	   Assert(X_2048 > X_768, "Failed with 2048 and 768 Bit.");
   	   Assert(X_2048 > X_582, "Failed with 2048 and 582 Bit.");
   	   Assert(X_2048 > X_1, "Failed with 2048 and 1 Bit.");
   	   Assert(X_2048 > X_0, "Failed with 2048 and 0 Bit.");
   	   Assert(X_1025 > X_1024, "Failed with 1025 and 1024 Bit.");
   	   Assert(X_1025 > X_768, "Failed with 1025 and 768 Bit.");
   	   Assert(X_1025 > X_582, "Failed with 1025 and 582 Bit.");
   	   Assert(X_1025 > X_1, "Failed with 1025 and 1 Bit.");
   	   Assert(X_1025 > X_0, "Failed with 1025 and 0 Bit.");
   	   Assert(X_1024 > X_768, "Failed with 1024 and 768 Bit.");
   	   Assert(X_1024 > X_582, "Failed with 1024 and 582 Bit.");
   	   Assert(X_1024 > X_1, "Failed with 1024 and 1 Bit.");
   	   Assert(X_1024 > X_0, "Failed with 1024 and 0 Bit.");
   	   Assert(X_768 > X_582, "Failed with 768 and 582 Bit.");
   	   Assert(X_768 > X_1, "Failed with 768 and 1 Bit.");
   	   Assert(X_768 > X_0, "Failed with 768 and 0 Bit.");
   	   Assert(X_582 > X_1, "Failed with 582 and 1 Bit.");
   	   Assert(X_582 > X_0, "Failed with 582 and 0 Bit.");
   	   Assert(X_1 > X_0, "Failed 1 and 0 Bit.");

   end Big_Number_Comp_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Comp_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Assert(X_4096 <= X_4096, "Failed with 4096 and 4096 Bit.");
   	   Assert(X_4095 <= X_4096, "Failed with 4095 and 4096 Bit.");
   	   Assert(X_3812 <= X_4096, "Failed with 3812 and 4096 Bit.");
   	   Assert(X_2048 <= X_4096, "Failed with 2048 and 4096 Bit.");
   	   Assert(X_1025 <= X_4096, "Failed with 1025 and 4096 Bit.");
   	   Assert(X_1024 <= X_4096, "Failed with 1024 and 4096 Bit.");
   	   Assert(X_768 <= X_4096, "Failed with 768 and 4096 Bit.");
   	   Assert(X_582 <= X_4096, "Failed with 582 and 4096 Bit.");
   	   Assert(X_1 <= X_4096, "Failed with 1 and 4096 Bit.");
   	   Assert(X_0 <= X_4096, "Failed with 0 and 4096 Bit.");
   	   Assert(X_3812 <= X_3812, "Failed with 3812 Bit.");
   	   Assert(X_2048 <= X_3812, "Failed with 2048 and 3812 Bit.");
   	   Assert(X_1025 <= X_3812, "Failed with 1025 and 3812 Bit.");
   	   Assert(X_1024 <= X_3812, "Failed with 1024 and 3812 Bit.");
   	   Assert(X_768 <= X_3812, "Failed with 768 and 3812 Bit.");
   	   Assert(X_582 <= X_3812, "Failed with 582 and 3812 Bit.");
   	   Assert(X_1 <= X_3812, "Failed with 1 and 3812 Bit.");
   	   Assert(X_0 <= X_3812, "Failed with 0 and 3812 Bit.");
   	   Assert(X_2048 <= X_2048, "Failed with 2048 Bit.");
   	   Assert(X_1025 <= X_2048, "Failed with 1025 and 2048 Bit.");
   	   Assert(X_1024 <= X_2048, "Failed with 1024 and 2048 Bit.");
   	   Assert(X_768 <= X_2048, "Failed with 768 and 2048 Bit.");
   	   Assert(X_582 <= X_2048, "Failed with 582 and 2048 Bit.");
   	   Assert(X_1 <= X_2048, "Failed with 1 and 2048 Bit.");
   	   Assert(X_0 <= X_2048, "Failed with 0 and 2048 Bit.");
   	   Assert(X_1025 <= X_1025, "Failed with 1025 Bit.");
   	   Assert(X_1024 <= X_1025, "Failed with 1024 and 1025 Bit.");
   	   Assert(X_768 <= X_1025, "Failed with 768 and 1025 Bit.");
   	   Assert(X_582 <= X_1025, "Failed with 582 and 1025 Bit.");
   	   Assert(X_1 <= X_1025, "Failed with 1 and 1025 Bit.");
   	   Assert(X_0 <= X_1025, "Failed with 0 and 1025 Bit.");
   	   Assert(X_1024 <= X_1024, "Failed with 1024 and 1024 Bit.");
   	   Assert(X_768 <= X_1024, "Failed with 768 and 1024 Bit.");
   	   Assert(X_582 <= X_1024, "Failed with 582 and 1024 Bit.");
   	   Assert(X_1 <= X_1024, "Failed with 1 and 1024 Bit.");
   	   Assert(X_0 <= X_1024, "Failed with 0 and 1024 Bit.");
   	   Assert(X_768 <= X_768, "Failed with 768 Bit.");
   	   Assert(X_582 <= X_768, "Failed with 582 and 768 Bit.");
   	   Assert(X_1 <= X_768, "Failed with 1 and 768 Bit.");
   	   Assert(X_0 <= X_768, "Failed with 0 and 768 Bit.");
   	   Assert(X_582 <= X_582, "Failed with 582 Bit.");
   	   Assert(X_1 <= X_582, "Failed with 1 and 582 Bit.");
   	   Assert(X_0 <= X_582, "Failed with 0 and 582 Bit.");
   	   Assert(X_1 <= X_1, "Failed with 1 Bit.");
   	   Assert(X_0 <= X_1, "Failed with 1 and 0 Bit.");
   	   Assert(X_0 <= X_0, "Failed with 0 Bit.");

   end Big_Number_Comp_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Comp_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Assert(X_4096 >= X_4096, "Failed with 4096 Bit.");
   	   Assert(X_4096 >= X_4095, "Failed with 4096 and 4095 Bit.");
   	   Assert(X_4096 >= X_3812, "Failed with 4096 and 3812 Bit.");
   	   Assert(X_4096 >= X_2048, "Failed with 4096 and 2048 Bit.");
   	   Assert(X_4096 >= X_1025, "Failed with 4096 and 1025 Bit.");
   	   Assert(X_4096 >= X_1024, "Failed with 4096 and 1024 Bit.");
   	   Assert(X_4096 >= X_768, "Failed with 4096 and 768 Bit.");
   	   Assert(X_4096 >= X_582, "Failed with 4096 and 582 Bit.");
   	   Assert(X_4096 >= X_1, "Failed with 4096 and 1 Bit.");
   	   Assert(X_4096 >= X_0, "Failed with 4096 and 0 Bit.");
   	   Assert(X_4095 >= X_4095, "Failed with 4095 Bit.");
   	   Assert(X_4095 >= X_3812, "Failed with 4095 and 3812 Bit.");
   	   Assert(X_4095 >= X_2048, "Failed with 4095 and 2048 Bit.");
   	   Assert(X_4095 >= X_1025, "Failed with 4095 and 1025 Bit.");
   	   Assert(X_4095 >= X_1024, "Failed with 4095 and 1024 Bit.");
   	   Assert(X_4095 >= X_768, "Failed with 4095 and 768 Bit.");
   	   Assert(X_4095 >= X_582, "Failed with 4095 and 582 Bit.");
   	   Assert(X_4095 >= X_1, "Failed with 4095 and 1 Bit.");
   	   Assert(X_4095 >= X_0, "Failed with 4095 and 0 Bit.");
   	   Assert(X_3812 >= X_3812, "Failed with 3812 Bit.");
   	   Assert(X_3812 >= X_2048, "Failed with 3812 and 2048 Bit.");
   	   Assert(X_3812 >= X_1025, "Failed with 3812 and 1025 Bit.");
   	   Assert(X_3812 >= X_1024, "Failed with 3812 and 1024 Bit.");
   	   Assert(X_3812 >= X_768, "Failed with 3812 and 768 Bit.");
   	   Assert(X_3812 >= X_582, "Failed with 3812 and 582 Bit.");
   	   Assert(X_3812 >= X_1, "Failed with 3812 and 1 Bit.");
   	   Assert(X_3812 >= X_0, "Failed with 3812 and 0 Bit.");
   	   Assert(X_2048 >= X_2048, "Failed with 2048 Bit.");
   	   Assert(X_2048 >= X_1025, "Failed with 2048 and 1025 Bit.");
   	   Assert(X_2048 >= X_1024, "Failed with 2048 and 1024 Bit.");
   	   Assert(X_2048 >= X_768, "Failed with 2048 and 768 Bit.");
   	   Assert(X_2048 >= X_582, "Failed with 2048 and 582 Bit.");
   	   Assert(X_2048 >= X_1, "Failed with 2048 and 1 Bit.");
   	   Assert(X_2048 >= X_0, "Failed with 2048 and 0 Bit.");
   	   Assert(X_1025 >= X_1025, "Failed with 1025 Bit.");
   	   Assert(X_1025 >= X_1024, "Failed with 1025 and 1024 Bit.");
   	   Assert(X_1025 >= X_768, "Failed with 1025 and 768 Bit.");
   	   Assert(X_1025 >= X_582, "Failed with 1025 and 582 Bit.");
   	   Assert(X_1025 >= X_1, "Failed with 1025 and 1 Bit.");
   	   Assert(X_1025 >= X_0, "Failed with 1025 and 0 Bit.");
   	   Assert(X_1024 >= X_1024, "Failed with 1024 Bit.");
   	   Assert(X_1024 >= X_768, "Failed with 1024 and 768 Bit.");
   	   Assert(X_1024 >= X_582, "Failed with 1024 and 582 Bit.");
   	   Assert(X_1024 >= X_1, "Failed with 1024 and 1 Bit.");
   	   Assert(X_1024 >= X_0, "Failed with 1024 and 0 Bit.");
   	   Assert(X_768 >= X_768, "Failed with 768 Bit.");
   	   Assert(X_768 >= X_582, "Failed with 768 and 582 Bit.");
   	   Assert(X_768 >= X_1, "Failed with 768 and 1 Bit.");
   	   Assert(X_768 >= X_0, "Failed with 768 and 0 Bit.");
   	   Assert(X_582 >= X_582, "Failed with 582 Bit.");
   	   Assert(X_582 >= X_1, "Failed with 582 and 1 Bit.");
   	   Assert(X_582 >= X_0, "Failed with 582 and 0 Bit.");
   	   Assert(X_1 >= X_1, "Failed with 1 Bit.");
   	   Assert(X_1 >= X_0, "Failed with 1 and 0 Bit.");
   	   Assert(X_0 >= X_0, "Failed with 0 Bit.");

   end Big_Number_Comp_Test5;

------------------------------------------------------------------------------------

end Test.Big_Number_Comp;
