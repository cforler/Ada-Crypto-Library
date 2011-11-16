with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Swap is

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
---------------------------- Register Big_Number Test 1 ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin

		Register_Routine(T, Big_Number_Test1'Access,"Swap with 4096 Bit");
		Register_Routine(T, Big_Number_Test2'Access,"Swap with 1024 and 1025 Bit");
		Register_Routine(T, Big_Number_Test3'Access,"Swap with 1024 and 768 Bit");
		Register_Routine(T, Big_Number_Test4'Access,"Swap with 1 and 0 Bit");

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
	   P := X_4096 - 1;
	   Z := X_4096;
	   R := X_4096 - 1;
	   Q := X_4096;

	   Swap(R,Q); 
   	   Assert((R = Z) and (Q = P), "1: Failed.");
   	   Assert((Z = R) and (P = Q), "2: Failed.");
   	   Assert((Z = X_4096) and (R = X_4096), "3: Failed.");
   	   Assert((Q = X_4096 - 1) and (P = X_4096 - 1), "4: Failed.");
   
   end Big_Number_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Q := X_1024;
	   P := X_1024;
	   R := X_1025;
	   Z := X_1025;
   	   
	   Swap(R,Q); 
   	   Assert((R = P) and (Q = Z), "1: Failed.");
   	   Assert((P = R) and (Z = Q), "2: Failed.");
   	   Assert((R = X_1024) and (Q = X_1025), "3: Failed.");
   	   Assert((P = X_1024) and (Z = X_1025), "4: Failed.");

   end Big_Number_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
	   Q := X_1024;
	   P := X_1024;
	   R := X_768;
	   Z := X_768;
   	   
	   Swap(R,Q); 
   	   Assert((R = P) and (Q = Z), "1: Failed.");
   	   Assert((P = R) and (Z = Q), "2: Failed.");
   	   Assert((R = X_1024) and (Q = X_768), "3: Failed.");
   	   Assert((P = X_1024) and (Z = X_768), "4: Failed.");
	   
	   Swap(Q,R); 
   	   Assert((R = Z) and (Q = P), "1: Reverse failed.");
   	   Assert((P = Q) and (Z = R), "2: Reverse failed.");
   	   Assert((R = X_768) and (Q = X_1024), "3: Reverse failed.");
   	   Assert((P = X_1024) and (Z = X_768), "4: Reverse failed.");
	   

   end Big_Number_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   P := To_Big_Unsigned("0");
	   Z := To_Big_Unsigned("1");
   	   
	   Swap(P,Z); 
   	   Assert((P = 1) and (Z = 0), "1: Failed.");
   	   Assert((P = X_1) and (Z = X_0), "2: Failed.");
   	   Assert((X_1 = P) and (X_0 = Z), "3: Failed.");
	   
	   Swap(Z,P); 
   	   Assert((P = 0) and (Z = 1), "1: Reverse failed.");
   	   Assert((P = X_0) and (Z = X_1), "2: Reverse failed.");
   	   Assert((X_0 = P) and (X_1 = Z), "3: Reverse Failed.");

   end Big_Number_Test4;

------------------------------------------------------------------------------------

end Test.Big_Number_Swap;
