with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Mult_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Multiplication is

-------------------------------------------------------------------------------
------------------------------ Type - Declaration -----------------------------
-------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(22400);
    use Big;
    use Big.Utils;

	use Big_Number_Mult_Results;

	X_0: Big_Unsigned := To_Big_Unsigned("0");
	X_2: Big_Unsigned := To_Big_Unsigned("2");
	A, B, C, D, E, F, G, H, I: Big_Unsigned;
	Result0, Result1, Result2, Result3, Result4, Result5, Result6, Result7,
	Result8: Big_Unsigned;
         
-------------------------------------------------------------------------------
------------------------------------ Results ----------------------------------
-------------------------------------------------------------------------------

	procedure Results is
	begin

		A := To_Big_Unsigned(Cons_1);
		B := To_Big_Unsigned(Cons_2);
		C := To_Big_Unsigned(Cons_3);
		D := To_Big_Unsigned(Cons_4);
		E := To_Big_Unsigned(Cons_5);
		F := To_Big_Unsigned(Cons_6);
		G := To_Big_Unsigned(Cons_7);
		H := To_Big_Unsigned(Cons_8);
		I := To_Big_Unsigned(Cons_9);
		Result0 := To_Big_Unsigned(Cons_R1);
		Result1 := To_Big_Unsigned(Cons_R2);
		Result2 := To_Big_Unsigned(Cons_R3);
		Result3 := To_Big_Unsigned(Cons_R4);
		Result4 := To_Big_Unsigned(Cons_R5);
		Result5 := To_Big_Unsigned(Cons_R6);
		Result6 := To_Big_Unsigned(Cons_R7);
		Result7 := To_Big_Unsigned(Cons_R8);
		Result8 := To_Big_Unsigned(Cons_R9);

	end Results;

-------------------------------------------------------------------------------
--------------------------- Register Big_Number Test 1 ------------------------
-------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, Big_Number_Test1'Access,"Schoolboy Multiplication");
		Register_Routine(T, Big_Number_Test2'Access,"Russian Peasant Multiplication");
		Register_Routine(T, Big_Number_Test3'Access,"Karatsuba Multiplication");
		Register_Routine(T, Big_Number_Test4'Access,"Parallel Karatsuba Multiplication");
		Register_Routine(T, Big_Number_Test5'Access,"Toom-Cook Multiplication");
		Register_Routine(T, Big_Number_Test6'Access,"Parallel Toom-Cook Multiplication");
		Register_Routine(T, Big_Number_Test7'Access,"Exponentiate with 2");
	end Register_Tests;

--------------------------------------------------------------------------------
------------------------------ Name Big_Number Test ----------------------------
--------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number Tests");
	end Name;

--------------------------------------------------------------------------------
---------------------------------- Start Tests ---------------------------------
------------------------------------ Test: 1 -----------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Results;
      Assert(A * A = Result0, "Failed with A.");
      Assert(B * B = Result1, "Failed with B.");
      Assert(C * C = Result2, "Failed with C.");
      Assert(D * D = Result3, "Failed with D.");
      Assert(E * E = Result4, "Failed with E.");
      Assert(F * F = Result5, "Failed with F.");
      Assert(G * G = Result6, "Failed with G.");
      Assert(H * H = Result7, "Failed with H.");
      Assert(I * I = Result8, "Failed with I.");
   end Big_Number_Test1;

--------------------------------------------------------------------------------
------------------------------------ Test 2 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Russ(X_0,X_0) = X_0, "Failed with 0.");
      Assert(Russ(A,A) = Result0, "Failed with A.");
      Assert(Russ(B,B) = Result1, "Failed with B.");
      Assert(Russ(C,C) = Result2, "Failed with C.");
      Assert(Russ(D,D) = Result3, "Failed with D.");
      Assert(Russ(E,E) = Result4, "Failed with E.");
      Assert(Russ(F,F) = Result5, "Failed with F.");
      Assert(Russ(G,G) = Result6, "Failed with G.");
      Assert(Russ(H,H) = Result7, "Failed with H.");
      Assert(Russ(I,I) = Result8, "Failed with I.");
   end Big_Number_Test2;

--------------------------------------------------------------------------------
------------------------------------ Test 3 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Karatsuba(X_0,X_0) = X_0, "Failed with 0.");
      Assert(Karatsuba(A,A) = Result0, "Failed with A.");
      Assert(Karatsuba(B,B) = Result1, "Failed with B.");
      Assert(Karatsuba(C,C) = Result2, "Failed with C.");
      Assert(Karatsuba(D,D) = Result3, "Failed with D.");
      Assert(Karatsuba(E,E) = Result4, "Failed with E.");
      Assert(Karatsuba(F,F) = Result5, "Failed with F.");
      Assert(Karatsuba(G,G) = Result6, "Failed with G.");
      Assert(Karatsuba(H,H) = Result7, "Failed with H.");
      Assert(Karatsuba(I,I) = Result8, "Failed with I.");
   end Big_Number_Test3;

--------------------------------------------------------------------------------
------------------------------------ Test 4 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Karatsuba_P(X_0,X_0) = X_0, "Failed with 0.");
      Assert(Karatsuba_P(A,A) = Result0, "Failed with A.");
      Assert(Karatsuba_P(B,B) = Result1, "Failed with B.");
      Assert(Karatsuba_P(C,C) = Result2, "Failed with C.");
      Assert(Karatsuba_P(D,D) = Result3, "Failed with D.");
      Assert(Karatsuba_P(E,E) = Result4, "Failed with E.");
      Assert(Karatsuba_P(F,F) = Result5, "Failed with F.");
      Assert(Karatsuba_P(G,G) = Result6, "Failed with G.");
      Assert(Karatsuba_P(H,H) = Result7, "Failed with H.");
      Assert(Karatsuba_P(I,I) = Result8, "Failed with I.");
   end Big_Number_Test4;

--------------------------------------------------------------------------------
------------------------------------ Test 5 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Toom_Cook(X_0,X_0) = X_0, "Failed with 0.");
      Assert(Toom_Cook(A,A) = Result0, "Failed with A.");
      Assert(Toom_Cook(B,B) = Result1, "Failed with B.");
      Assert(Toom_Cook(C,C) = Result2, "Failed with C.");
      Assert(Toom_Cook(D,D) = Result3, "Failed with D.");
      Assert(Toom_Cook(E,E) = Result4, "Failed with E.");
      Assert(Toom_Cook(F,F) = Result5, "Failed with F.");
      Assert(Toom_Cook(G,G) = Result6, "Failed with G.");
      Assert(Toom_Cook(H,H) = Result7, "Failed with H.");
      Assert(Toom_Cook(I,I) = Result8, "Failed with I.");
   end Big_Number_Test5;

--------------------------------------------------------------------------------
------------------------------------ Test 6 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(Toom_Cook_P(X_0,X_0) = X_0, "Failed with 0.");
      Assert(Toom_Cook_P(A,A) = Result0, "Failed with A.");
      Assert(Toom_Cook_P(B,B) = Result1, "Failed with B.");
      Assert(Toom_Cook_P(C,C) = Result2, "Failed with C.");
      Assert(Toom_Cook_P(D,D) = Result3, "Failed with D.");
      Assert(Toom_Cook_P(E,E) = Result4, "Failed with E.");
      Assert(Toom_Cook_P(F,F) = Result5, "Failed with F.");
      Assert(Toom_Cook_P(G,G) = Result6, "Failed with G.");
      Assert(Toom_Cook_P(H,H) = Result7, "Failed with H.");
      Assert(Toom_Cook_P(I,I) = Result8, "Failed with I.");
   end Big_Number_Test6;

--------------------------------------------------------------------------------
------------------------------------ Test 7 ------------------------------------
--------------------------------------------------------------------------------

   procedure Big_Number_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Assert(A**X_2 = Result0, "Failed with A.");
      Assert(B**X_2 = Result1, "Failed with B.");
      Assert(C**X_2 = Result2, "Failed with C.");
      Assert(D**X_2 = Result3, "Failed with D.");
      Assert(E**X_2 = Result4, "Failed with E.");
      Assert(F**X_2 = Result5, "Failed with F.");
      Assert(G**X_2 = Result6, "Failed with G.");
      Assert(H**X_2 = Result7, "Failed with H.");
      Assert(I**X_2 = Result8, "Failed with I.");
   end Big_Number_Test7;

--------------------------------------------------------------------------------

end Test.Big_Number_Multiplication;
