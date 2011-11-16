with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Mult2_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Mult2 is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(22400);
    use Big;
    use Big.Utils;
    
	use Big_Number_Mult2_Results;

	A, B, C, D, E, F, G, H, I, A1, B1, C1, D1, E1, F1, G1, H1, I1, 
	Result0, Result1, Result2, Result3, Result4, Result5, Result6, Result7, Result8
	: Big_Unsigned;
    
    procedure Constants is
	begin

		A := To_Big_Unsigned(Cons_1);
		A1 := To_Big_Unsigned(Cons_1_1);
		Result0 := To_Big_Unsigned(Cons_R1);
		B := To_Big_Unsigned(Cons_2);
		B1 := To_Big_Unsigned(Cons_2_1);
		Result1 := To_Big_Unsigned(Cons_R2);
		C := To_Big_Unsigned(Cons_3);
		C1 := To_Big_Unsigned(Cons_3_1);
		Result2 := To_Big_Unsigned(Cons_R3);
		D := To_Big_Unsigned(Cons_4);
		D1 := To_Big_Unsigned(Cons_4_1);
		Result3 := To_Big_Unsigned(Cons_R4);
		E := To_Big_Unsigned(Cons_5);
		E1 := To_Big_Unsigned(Cons_5_1);
		Result4 := To_Big_Unsigned(Cons_R5);
		F := To_Big_Unsigned(Cons_6);
		F1 := To_Big_Unsigned(Cons_6_1);
		Result5 := To_Big_Unsigned(Cons_R6);
		G := To_Big_Unsigned(Cons_7);
		G1 := To_Big_Unsigned(Cons_7_1);
		Result6 := To_Big_Unsigned(Cons_R7);
		H := To_Big_Unsigned(Cons_8);
		H1 := To_Big_Unsigned(Cons_8_1);
		Result7 := To_Big_Unsigned(Cons_R8);
		I := To_Big_Unsigned(Cons_9);
		I1 := To_Big_Unsigned(Cons_9_1);
		Result8 := To_Big_Unsigned(Cons_R9);

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number Test 1 ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, Big_Number_Test1'Access,"Schoolboy Multiplication");
		Register_Routine(T, Big_Number_Test2'Access,"Russian Peasant Multiplication");
		Register_Routine(T, Big_Number_Test3'Access,"Karatsuba Multiplication");
		Register_Routine(T, Big_Number_Test4'Access,"Parallel Karatsuba Multiplication");
		Register_Routine(T, Big_Number_Test5'Access,"Toom-Cook Multiplication");
		Register_Routine(T, Big_Number_Test6'Access,"Parallel Toom-Cook Multiplication");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number Test -------------------------------
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
      Assert(A * A1 = Result0, "Failed with A.");
      Assert(B * B1 = Result1, "Failed with B.");
      Assert(C * C1 = Result2, "Failed with C.");
      Assert(D * D1 = Result3, "Failed with D.");
      Assert(E * E1 = Result4, "Failed with E.");
      Assert(F * F1 = Result5, "Failed with F.");
      Assert(G * G1 = Result6, "Failed with G."); 
      Assert(H * H1 = Result7, "Failed with H.");
      Assert(I * I1 = Result8, "Failed with I.");
   end Big_Number_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Russ(A,A1) = Result0, "Failed with A.");
      Assert(Russ(B,B1) = Result1, "Failed with B.");
      Assert(Russ(C,C1) = Result2, "Failed with C.");
      Assert(Russ(D,D1) = Result3, "Failed with D.");
      Assert(Russ(E,E1) = Result4, "Failed with E.");
      Assert(Russ(F,F1) = Result5, "Failed with F.");
      Assert(Russ(G,G1) = Result6, "Failed with G.");
      Assert(Russ(H,H1) = Result7, "Failed with H.");
      Assert(Russ(I,I1) = Result8, "Failed with I.");
   end Big_Number_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Karatsuba(A,A1) = Result0, "Failed with A.");
      Assert(Karatsuba(B,B1) = Result1, "Failed with B.");
      Assert(Karatsuba(C,C1) = Result2, "Failed with C.");
      Assert(Karatsuba(D,D1) = Result3, "Failed with D.");
      Assert(Karatsuba(E,E1) = Result4, "Failed with E.");
      Assert(Karatsuba(F,F1) = Result5, "Failed with F.");
      Assert(Karatsuba(G,G1) = Result6, "Failed with G.");
      Assert(Karatsuba(H,H1) = Result7, "Failed with H.");
      Assert(Karatsuba(I,I1) = Result8, "Failed with I.");
   end Big_Number_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Karatsuba_P(A,A1) = Result0, "Failed with A.");
      Assert(Karatsuba_P(B,B1) = Result1, "Failed with B.");
      Assert(Karatsuba_P(C,C1) = Result2, "Failed with C.");
      Assert(Karatsuba_P(D,D1) = Result3, "Failed with D.");
      Assert(Karatsuba_P(E,E1) = Result4, "Failed with E.");
      Assert(Karatsuba_P(F,F1) = Result5, "Failed with F.");
      Assert(Karatsuba_P(G,G1) = Result6, "Failed with G.");
      Assert(Karatsuba_P(H,H1) = Result7, "Failed with H.");
      Assert(Karatsuba_P(I,I1) = Result8, "Failed with I.");
   end Big_Number_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Toom_Cook(A,A1) = Result0, "Failed with A.");
      Assert(Toom_Cook(B,B1) = Result1, "Failed with B.");
      Assert(Toom_Cook(C,C1) = Result2, "Failed with C.");
      Assert(Toom_Cook(D,D1) = Result3, "Failed with D.");
      Assert(Toom_Cook(E,E1) = Result4, "Failed with E.");
      Assert(Toom_Cook(F,F1) = Result5, "Failed with F.");
      Assert(Toom_Cook(G,G1) = Result6, "Failed with G.");
      Assert(Toom_Cook(H,H1) = Result7, "Failed with H.");
      Assert(Toom_Cook(I,I1) = Result8, "Failed with I.");
   end Big_Number_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Toom_Cook_P(A,A1) = Result0, "Failed with A.");
      Assert(Toom_Cook_P(B,B1) = Result1, "Failed with B.");
      Assert(Toom_Cook_P(C,C1) = Result2, "Failed with C.");
      Assert(Toom_Cook_P(D,D1) = Result3, "Failed with D.");
      Assert(Toom_Cook_P(E,E1) = Result4, "Failed with E.");
      Assert(Toom_Cook_P(F,F1) = Result5, "Failed with F.");
      Assert(Toom_Cook_P(G,G1) = Result6, "Failed with G.");
      Assert(Toom_Cook_P(H,H1) = Result7, "Failed with H.");
      Assert(Toom_Cook_P(I,I1) = Result8, "Failed with I.");
   end Big_Number_Test6;

------------------------------------------------------------------------------------

end Test.Big_Number_Mult2;
