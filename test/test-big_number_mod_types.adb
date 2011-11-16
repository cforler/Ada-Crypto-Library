with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Mod_Types is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

   package Big is new Crypto.Types.Big_Numbers(4096);
   use Big;
   use Big.Utils;
   use Crypto.Types;
   use Big_Number_Constants;	
    
    X, Result, X_4096, X_4095, X_3812, X_1025, X_1024, X_768, X_1, X_0: Big_Unsigned;
    K, L, M, N, O: Mod_Type;
	
------------------------------------------------------------------------------------
----------------------------------- Constants --------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
		
   	    -- 2^ 32 - 1
   	    K := 4294967295;
   	    -- 2^31
	    L := 2147483648;
	    -- 2^16
   	    M := 65536;
   	    -- 2^15
   	    N := 32768;
   	    -- 2^12
   	    O := 4096;
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768 := To_Big_Unsigned(Cons_768);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big Number Mod Type Tests --------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Type_Test1'Access,"Mod with Mod Type and 4096 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test2'Access,"Mod with Mod Type and 4095 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test3'Access,"Mod with Mod Type and 2048 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test4'Access,"Mod with Mod Type and 1025 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test5'Access,"Mod with Mod Type and 1024 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test6'Access,"Mod with Mod Type and 768 Bit");
		Register_Routine(T, Big_Number_Mod_Type_Test7'Access,"And, substraction and negation with Mod Type");

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

   procedure Big_Number_Mod_Type_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   Constants;

	   X := X_4096 mod K;
	   Assert(X = X_0, "Failed with Mod Type 2**32 - 1.");

	   Result := To_Big_Unsigned("2147483647");
	   X := X_4096 mod L;
	   Assert(X = Result, "Failed with Mod Type 2**31.");

	   Result := To_Big_Unsigned("65535");
	   X := X_4096 mod M;
	   Assert(X = Result, "Failed with Mod Type 2**16.");

	   Result := To_Big_Unsigned("32767");
	   X := X_4096 mod N;
	   Assert(X = Result, "Failed with Mod Type 2**15.");

	   Result := To_Big_Unsigned("4095");
	   X := X_4096 mod O;
	   Assert(X = Result, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------
   
   procedure Big_Number_Mod_Type_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

	   X := X_4095 mod K;
	   Assert(X = L, "Failed with Mod Type 2**32 - 1.");

	   X := X_4095 mod L;
	   Assert(X = X_0, "Failed with Mod Type 2**31.");

	   X := X_4095 mod M;
	   Assert(X = X_0, "Failed with Mod Type 2**16.");

	   X := X_4095 mod N;
	   Assert(X = X_0, "Failed with Mod Type 2**15.");

	   X := X_4095 mod O;
	   Assert(X = X_0, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("2402063328");
	   X := X_3812 mod K;
	   Assert(X = Result, "Failed with Mod Type 2**32 - 1.");

	   X := X_3812 mod L;
	   Assert(X = X_0, "Failed with Mod Type 2**31.");

	   X := X_3812 mod M;
	   Assert(X = X_0, "Failed with Mod Type 2**16.");

	   X := X_3812 mod N;
	   Assert(X = X_0, "Failed with Mod Type 2**15.");

	   X := X_3812 mod O;
	   Assert(X = X_0, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test3;

------------------------------------------------------------------------------------
------------------------------------- Test 4 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   X := X_1025 mod K;
	   Assert(X = X_1, "Failed with Mod Type 2**32 - 1.");

	   X := X_1025 mod L;
	   Assert(X = X_0, "Failed with Mod Type 2**31.");

	   X := X_1025 mod M;
	   Assert(X = X_0, "Failed with Mod Type 2**16.");

	   X := X_1025 mod N;
	   Assert(X = X_0, "Failed with Mod Type 2**15.");

	   X := X_1025 mod O;
	   Assert(X = X_0, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test4;

------------------------------------------------------------------------------------
------------------------------------- Test 5 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
	   X := X_1024 mod K;
	   Assert(X = X_0, "Failed with Mod Type 2**32 - 1.");

	   Result := To_Big_Unsigned("2147483647");
	   X := X_1024 mod L;
	   Assert(X = Result, "Failed with Mod Type 2**31.");

	   Result := To_Big_Unsigned("65535");
	   X := X_1024 mod M;
	   Assert(X = Result, "Failed with Mod Type 2**16.");

	   Result := To_Big_Unsigned("32767");
	   X := X_1024 mod N;
	   Assert(X = Result, "Failed with Mod Type 2**15.");

	   Result := To_Big_Unsigned("4095");
	   X := X_1024 mod O;
	   Assert(X = Result, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test5;

------------------------------------------------------------------------------------
------------------------------------- Test 6 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("289617365");
	   X := X_768 mod K;
	   Assert(X = Result, "Failed with Mod Type 2**32 - 1.");

	   Result := To_Big_Unsigned("1542336506");
	   X := X_768 mod L;
	   Assert(X = Result, "Failed with Mod Type 2**31.");

	   Result := To_Big_Unsigned("12282");
	   X := X_768 mod M;
	   Assert(X = Result, "Failed with Mod Type 2**16.");

	   X := X_768 mod N;
	   Assert(X = Result, "Failed with Mod Type 2**15.");

	   Result := To_Big_Unsigned("4090");
	   X := X_768 mod O;
	   Assert(X = Result, "Failed with Mod Type 2**12.");

   end Big_Number_Mod_Type_Test6;

------------------------------------------------------------------------------------
------------------------------------- Test 7 ---------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Type_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

	   Result := To_Big_Unsigned("4294967295");
   	   X := X_4096 and K;
   	   Assert(X = Result, "Failed with 4096 Bit and 2**32 - 1.");
	   
	   Result := To_Big_Unsigned("2147483648");
   	   X := X_4096 and L;
   	   Assert(X = Result, "Failed with 4096 Bit and 2**31.");
	   
---------------------------------------------------------------------------
   	   
   	   X := L and X_4096;
   	   Assert(X = L, "Failed with 31 and 4096 Bit.");
	   
---------------------------------------------------------------------------

	   Result := To_Big_Unsigned("10443888814131525066917527107166243825799642490473" &
	   "8378038423348328395390797155745684882681193499755834089010671443926283798757" &
	   "3438185793607263236087851365277945956976543709998340361590134383718314428070" &
	   "0118559462263763188393977127456723346843445866174968079087058037040712840487" &
	   "4011860911446797778359802900668693897688178778594690563019026094059957945343" &
	   "2823469303026696443059025015972399867714215541693835559885291486318237914434" &
	   "4967340878118726394964751001890413490084170616750936683338505510329720882695" &
	   "5076998361636941193301521379682583718809183365675122131849284636812555022599" &
	   "8300412344784862595674492194617023806505913245610825731835380087608622102834" &
	   "2701976982023131690176780066751954850799216364193702853751247840149071591354" &
	   "5998279051339961155179427110683113409058427288427979155484978295432353451706" &
	   "5223269061394905987693002122963395687782878948440616007412945674919823050571" &
	   "6423771548163213806310459029161369267083428564407304478999719017814657634732" &
	   "2385026725305989979599609079946920177462376520035756667474650726787381358710" &
	   "2316468939668516183820520793686919998581941111488810524490438886213743075177" &
	   "7949447718001115016225461430801602533355901899103689174675181878662251564409" &
	   "04448199211744312718596926815493753633099781");
	   X := K - X_768;
   	   Assert(X = Result, "Failed with Mod Type 2**31 and 4096 Bit.");

	   X := - Result;
	   Result := X_768 - K - 2;
	   Assert(X = Result, "Negation failed.");

	   X := - X_0;
	   Assert(X = 0, "Negation failed.");
   
   end Big_Number_Mod_Type_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Mod_Types;
