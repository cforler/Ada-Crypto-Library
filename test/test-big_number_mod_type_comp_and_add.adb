with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Mod_Type_Comp_and_Add is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

   package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    use Big.Mod_Utils;
    use Crypto.Types;	
    use Big_Number_Constants;
    
    Result, X, X_4096, X_32, X_31, X_16, X_15, X_12, X_1, X_0: Big_Unsigned;
    K, L, M, N, O, P, Q: Mod_Type;
	
------------------------------------------------------------------------------------
----------------------------------- Constants --------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
		
		X_4096 := To_Big_Unsigned(Cons_4096);
   		-- 2^ 32 - 1
   		K := 4294967295;
		X_32 := To_Big_Unsigned("4294967295");
   		-- 2^31
		L := 2147483648;
		X_31 := To_Big_Unsigned("2147483648");
		-- 2^16
   		M := 65536;
		X_16 := To_Big_Unsigned("65536");
   		-- 2^15
   		N := 32768;
		X_15 := To_Big_Unsigned("32768");
   		-- 2^12
		X_12 := To_Big_Unsigned("4096");
   		O := 4096;
		X_1 := To_Big_Unsigned("1");
   		P := 1;
		X_0 := To_Big_Unsigned("0");
   		Q := 0;

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number_Mod_Type Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Type_Test1'Access,"Simple comparison with Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test2'Access,"Simple comparison bigger than with Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test3'Access,"Simple comparison bigger or equal than with Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test4'Access,"Simple comparison smaller or equal than with Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test5'Access,"Simple comparison smaller than with Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test6'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test7'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test8'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test9'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test10'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test11'Access,"Addition with Big_Unsigned and Mod Type");
		Register_Routine(T, Big_Number_Mod_Type_Test12'Access,"Addition with Big_Unsigned and Mod Type");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number_Mod_Type Test ------------------------------
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

   procedure Big_Number_Mod_Type_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   Constants;

	   Assert(X_32 = K, "Failed with 2**32 - 1.");
	   Assert(K = X_32, "Failed with 2**32 - 1.");
	   
	   Assert(X_31 = L, "Failed with 2**31.");
	   Assert(L = X_31, "Failed with 2**31.");

	   Assert(X_16 = M, "Failed with 2**16.");
	   Assert(M = X_16, "Failed with 2**16.");

	   Assert(X_15 = N, "Failed with 2**15.");
	   Assert(N = X_15, "Failed with 2**15.");
	   
	   Assert(X_12 = O, "Failed with 2**12.");
	   Assert(O = X_12, "Failed with 2**12.");
   
   end Big_Number_Mod_Type_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Assert(X_4096 > K, "Failed with 32 and 4096 Bit.");
	   Assert(X_32 > L, "Failed with 2**31 and 2**32-1.");
	   Assert(X_32 > M, "Failed with 2**16 and 2**32-1.");
	   Assert(X_32 > N, "Failed with 2**15 and 2**32-1.");
	   Assert(X_32 > O, "Failed with 2**12 and 2**32-1.");

	   Assert((K > X_32) = FALSE, "Failed with 2**32 - 1.");
	   Assert(K > X_31, "Failed with 2**32-1 and 2**31.");
	   Assert(K > X_16, "Failed with 2**32-1 and 2**16.");
	   Assert(K > X_15, "Failed with 2**32-1 and 2**15.");
	   Assert(K > X_12, "Failed with 2**32-1 and 2**12.");

   end Big_Number_Mod_Type_Test2;

------------------------------------------------------------------------------------
------------------------------------- Test 3 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Assert(X_4096 >= K, "Failed with 32 and 4096 Bit.");
	   Assert(X_32 >= K, "Failed with 2**32 - 1.");
	   Assert(X_32 >= L, "Failed with 2**32 - 1 and 2**31.");
	   Assert(X_32 >= M, "Failed with 2**32 - 1 and 2**16.");
	   Assert(X_32 >= N, "Failed with 2**32 - 1 and 2**15.");
	   Assert(X_32 >= O, "Failed with 2**32 - 1 and 2**12.");

	   Assert(K >= X_32, "Failed with 2**32 - 1.");
	   Assert(K >= X_31, "Failed with 2**32 - 1 and 2**31.");
	   Assert(K >= X_16, "Failed with 2**32 - 1 and 2**16.");
	   Assert(K >= X_15, "Failed with 2**32 - 1 and 2**15.");
	   Assert(K >= X_12, "Failed with 2**32 - 1 and 2**12.");

   end Big_Number_Mod_Type_Test3;

------------------------------------------------------------------------------------
------------------------------------- Test 4 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
	   Assert(K <= X_4096, "Failed with 2**31 - 1.");
	   Assert(K <= X_32, "Failed with 2**31 - 1.");
	   Assert(L <= X_32, "Failed with 2**31.");
	   Assert(M <= X_32, "Failed with 2**16.");
	   Assert(N <= X_32, "Failed with 2**15.");
	   Assert(O <= X_32, "Failed with 2**12.");

	   Assert(X_32 <= K, "Failed with 2**32 - 1.");
	   Assert(X_31 <= K, "Failed with 2**31.");
	   Assert(X_16 <= K, "Failed with 2**16.");
	   Assert(X_15 <= K, "Failed with 2**15.");
	   Assert(X_12 <= K, "Failed with 2**12.");
   
   end Big_Number_Mod_Type_Test4;

------------------------------------------------------------------------------------
------------------------------------- Test 5 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Assert((K < X_32) = False, "Failed with 2**32 - 1.");
	   Assert(L < X_32, "Failed with 2**32 - 1 and 2**31.");
	   Assert(M < X_32, "Failed with 2**32 - 1 and 2**16.");
	   Assert(N < X_32, "Failed with 2**32 - 1 and 2**15.");
	   Assert(O < X_32, "Failed with 2**32 - 1 and 2**12.");

	   Assert((X_32 < K) = FALSE, "Failed with 2**32 - 1.");
	   Assert(X_31 < K, "Failed with 2**31 and 2**32 - 1.");
	   Assert(X_16 < K, "Failed with 2**16 and 2**32 - 1.");
	   Assert(X_15 < K, "Failed with 2**15 and 2**32 - 1.");
	   Assert(X_12 < K, "Failed with 2**12 and 2**32 - 1.");
   
   end Big_Number_Mod_Type_Test5;

------------------------------------------------------------------------------------
------------------------------------- Test 6 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   Result := To_Big_Unsigned("4294967294");
   	   X := X_4096 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 4096 Bit.");
   	   X := K + X_4096;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 4096 Bit.");

   	   Result := To_Big_Unsigned("2147483647");
   	   X := X_4096 + L;
   	   Assert(Result = X, "Failed with Mod Type 2**31 and 4096 Bit.");
	   X := L + X_4096;
   	   Assert(Result = X, "Failed with Mod Type 2**31 and 4096 Bit.");
   	   
   	   Result := To_Big_Unsigned("65535");
   	   X := X_4096 + M;
   	   Assert(Result = X, "Failed with Mod Type 2**16 and 4096 Bit.");
   	   X := M + X_4096;
   	   Assert(Result = X, "Failed with Mod Type 2**16 and 4096 Bit.");

   	   Result := To_Big_Unsigned("32767");
   	   X := X_4096 + N;
   	   Assert(Result = X, "Failed with Mod Type 2**15 and 4096 Bit.");
   	   X := N + X_4096;
   	   Assert(Result = X, "Failed with Mod Type 2**15 and 4096 Bit.");
   	   
   	   Result := To_Big_Unsigned("4095");
   	   X := X_4096 + O;
   	   Assert(Result = X, "Failed with Mod Type 2**12 and 4096 Bit.");
   	   X := O + X_4096;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 4096 Bit.");

   end Big_Number_Mod_Type_Test6;

------------------------------------------------------------------------------------
------------------------------------- Test 7 ---------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("8589934590");
   	   X := X_32 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1.");
   	   X := K + X_32;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1.");

	   Result := To_Big_Unsigned("6442450943");
   	   X := X_32 + L;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**31.");
   	   X := L + X_32;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**32 - 1.");
   	   
	   Result := To_Big_Unsigned("4295032831");
   	   X := X_32 + M;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**16.");
   	   X := M + X_32;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**32 - 1.");

	   Result := To_Big_Unsigned("4295000063");
   	   X := X_32 + N;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**15.");
   	   X := N + X_32;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**32 - 1.");

	   Result := To_Big_Unsigned("4294971391");
   	   X := X_32 + O;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**12.");
   	   X := O + X_32;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**32 - 1.");

   end Big_Number_Mod_Type_Test7;

------------------------------------------------------------------------------------
------------------------------------- Test 8 ---------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test8(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("6442450943");
   	   X := X_31 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**32 - 1.");
   	   X := K + X_31;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**31.");
	   
	   Result := To_Big_Unsigned("4295032831");
   	   X := X_16 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**32 - 1.");
   	   X := K + X_16;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**16.");

	   Result := To_Big_Unsigned("4295000063");
   	   X := K + X_15;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**15.");
   	   X := X_15 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**32 - 1.");

	   Result := To_Big_Unsigned("4294971391");
   	   X := K + X_12;
   	   Assert(X = Result, "Failed with Mod Type 2**32 - 1 and 2**12.");
   	   X := X_12 + K;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**32 - 1.");

   end Big_Number_Mod_Type_Test8;

------------------------------------------------------------------------------------
------------------------------------- Test 9 ---------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test9(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("4294967296");
   	   X := X_31 + L;
   	   Assert(X = Result, "Failed with Mod Type 2**31.");
   	   X := L + X_31;
   	   Assert(X = Result, "Failed with Mod Type 2**31.");
	   
	   Result := To_Big_Unsigned("2147549184");
   	   X := X_16 + L;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**31.");
   	   X := L + X_16;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**16.");

	   Result := To_Big_Unsigned("2147516416");
   	   X := L + X_15;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**16.");
   	   X := X_15 + L;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**31.");

	   Result := To_Big_Unsigned("2147487744");
   	   X := L + X_12;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**12.");
   	   X := X_12 + L;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**31.");

   end Big_Number_Mod_Type_Test9;
	   
------------------------------------------------------------------------------------
------------------------------------- Test 10 --------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test10(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("2147549184");
   	   X := X_31 + M;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**16.");
   	   X := M + X_31;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**31.");
	   
	   Result := To_Big_Unsigned("131072");
   	   X := X_16 + M;
   	   Assert(X = Result, "Failed with Mod Type 2**16.");
   	   X := M + X_16;
   	   Assert(X = Result, "Failed with Mod Type 2**16.");

	   Result := To_Big_Unsigned("98304");
   	   X := M + X_15;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**15.");
   	   X := X_15 + M;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**16.");

	   Result := To_Big_Unsigned("69632");
   	   X := M + X_12;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**12.");
   	   X := X_12 + M;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**16.");

   end Big_Number_Mod_Type_Test10;

------------------------------------------------------------------------------------
------------------------------------- Test 11 --------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test11(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("2147516416");
   	   X := X_31 + N;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**15.");
   	   X := N + X_31;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**31.");
	   
	   Result := To_Big_Unsigned("98304");
   	   X := X_16 + N;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**15.");
   	   X := N + X_16;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**16.");

   	   X := N + X_15;
   	   Assert(X = X_16, "Failed with Mod Type 2**15.");
   	   Assert(X = M, "Failed with Mod Type 2**15.");
   	   X := X_15 + N;
   	   Assert(X = X_16, "Failed with Mod Type 2**15.");
   	   Assert(X = M, "Failed with Mod Type 2**15.");

	   Result := To_Big_Unsigned("36864");
   	   X := N + X_12;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**12.");
   	   X := X_12 + N;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**16.");

   end Big_Number_Mod_Type_Test11;
	   
------------------------------------------------------------------------------------
------------------------------------- Test 12 --------------------------------------
------------------------------------------------------------------------------------
   	   
   procedure Big_Number_Mod_Type_Test12(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("2147487744");
   	   X := X_31 + O;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 2**12.");
   	   X := O + X_31;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**31.");
	   
	   Result := To_Big_Unsigned("69632");
   	   X := X_16 + O;
   	   Assert(X = Result, "Failed with Mod Type 2**16 and 2**12.");
   	   X := O + X_16;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**16.");

	   Result := To_Big_Unsigned("36864");
   	   X := O + X_15;
   	   Assert(X = Result, "Failed with Mod Type 2**12 and 2**15.");
   	   X := X_15 + O;
   	   Assert(X = Result, "Failed with Mod Type 2**15 and 2**12.");

	   Result := To_Big_Unsigned("8192");
   	   X := O + X_12;
   	   Assert(X = Result, "Failed with Mod Type 2**12.");
   	   X := X_12 + O;
   	   Assert(X = Result, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test12;

------------------------------------------------------------------------------------

end Test.Big_Number_Mod_Type_Comp_and_Add;
