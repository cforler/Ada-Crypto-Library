with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;
with Big_Number_Sub_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Sub is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;

	use Big_Number_Constants;
	use Big_Number_Sub_Results;

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
---------------------------- Register Big_Number5 Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number5_Test1'Access,"Big_Number5_Test1.");
		Register_Routine(T, Big_Number5_Test2'Access,"Big_Number5_Test2.");
		Register_Routine(T, Big_Number5_Test3'Access,"Big_Number5_Test3.");
		Register_Routine(T, Big_Number5_Test4'Access,"Big_Number5_Test4.");
		Register_Routine(T, Big_Number5_Test5'Access,"Big_Number5_Test5.");
		Register_Routine(T, Big_Number5_Test6'Access,"Big_Number5_Test6.");
		Register_Routine(T, Big_Number5_Test7'Access,"Big_Number5_Test7.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number5 Test ------------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number 5 Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

  	   Constants;
  	   X := X_4096 - X_4096;
   	   Assert(X = X_0, "Substraction with Big Number 4096 Bit failed.");
   	   
   	   X := X_4096 - X_3812;
	   Result := To_Big_Unsigned(Result_0);
   	   Assert(X = Result, "Substraction with Big Number 4096 and 3812 Bit failed.");
   	   
   	   X := X_4096 - X_1025;
	   Result := To_Big_Unsigned(Result_1);
   	   Assert(X = Result, "Substraction with Big Number 4096 and 1025 Bit failed.");
   	   
   	   X := X_4096 - X_1024;
	   Result := Result + 1;
   	   Assert(X = Result, "Substraction with Big Number 4096 and 1024 Bit failed.");

   	   X := X_4096 - X_768;
	   Result := To_Big_Unsigned(Result_2);
   	   Assert(X = Result, "Substraction with Big Number 4096 and 768 Bit failed.");
   	   
   	   X := X_4096 - X_1;
	   Result := To_Big_Unsigned(Result_3);
   	   Assert(X = Result, "Substraction with Big Number 4096 and 1 Bit failed.");
   	   X := X_4096 - X_0;
   	   Assert(X = X_4096, "Substraction with Big Number 4096 Bit failed.");

   end Big_Number5_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   -- Big Numbers are always positiv: Wrap around
   	   -- Reverse to Plus operator
   	   X := X_3812 - X_4096;
   	   Result := X_3812 + 1;
   	   Assert(X = Result, "Substraction with Big Number 4096 and 3812 Bit failed.");
   	   
   	   X := X_3812 - X_3812;
   	   Assert(X = X_0, "Substraction with Big Number 3812 Bit failed.");
   	   
   	   X := X_3812 - X_1025;
	   Result := To_Big_Unsigned(Result_4);
   	   Assert(X = Result, "Substraction with Big Number 3812 and 1025 Bit failed.");
   	   
   	   X := X_3812 - X_1024;
	   Result := Result + 1;
   	   Assert(X = Result, "Substraction with Big Number 3812 and 1024 Bit failed.");

   	   X := X_3812 - X_768;
	   Result := To_Big_Unsigned(Result_5);
   	   Assert(X = Result, "Substraction with Big Number 3812 and 768 Bit failed.");

   	   X := X_3812 - X_1;
	   Result := X_3812 - 1;
   	   Assert(X = Result, "Substraction with Big Number 3812 and 1 Bit failed.");

   	   X := X_3812 - X_0;
   	   Assert(X = X_3812, "Substraction with Big Number 3812 and 0 Bit failed.");

   end Big_Number5_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1025 - X_4096;
   	   Result := X_1025 + 1;
   	   Assert(X = Result, "Substraction with Big Number 4096 and 1025 Bit failed.");
   	   
--   	   X := X_1025 - X_3812;
--   	   Assert(X = Result, "Substraction with Big Number 3812 and 1025 Bit failed.");
   	   
   	   X := X_1025 - X_1025;
   	   Assert(X = X_0, "Substraction with Big Number 1025 Bit failed.");
   	   
   	   X := X_1025 - X_1024;
   	   Assert(X = X_1, "Substraction with Big Number 1025 and 1024 Bit failed.");

   	   X := X_1025 - X_768;
	   Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805499910614616176541494793110664856993854276661" &
	   "0476507167763905715401610537531432459689392469469149975456050885280388106" &
	   "5229507854060477097600358273946971256866252106599110502867800024432989559" &
	   "9767364719649356344657957463482970408079366");
   	   Assert(X = Result, "Substraction with Big Number 1025 and 768 Bit failed.");

   	   X := X_1025 - X_1;
   	   Assert(X = X_1024, "Substraction with Big Number 3812 and 1 Bit failed.");

   	   X := X_1025 - X_0;
   	   Assert(X = X_1025, "Substraction with Big Number 3812 and 0 Bit failed.");

   end Big_Number5_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := X_1024 - X_1024;
   	   Assert(X = X_0, "Substraction with Big Number 1024 Bit failed.");

   	   X := X_1024 - X_768;
	   Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805499910614616176541494793110664856993854276661" & 
	   "0476507167763905715401610537531432459689392469469149975456050885280388106" & 
	   "5229507854060477097600358273946971256866252106599110502867800024432989559" & 
	   "9767364719649356344657957463482970408079365");
   	   Assert(X = Result, "Substraction with Big Number 1025 and 768 Bit failed.");

   	   X := X_1024 - X_1;
	   Result := To_Big_Unsigned("179769313486231590772930519078902473361797697" &
	   "89423065727343008115773267580550096313270847732240753602112011387987139" &
	   "33576587897688144166224928474306394741243777678934248654852763022196012" &
	   "46094119453082952085005768838150682342462881473913110540827237163350510" &
	   "684586298239947245938479716304835356329624224137214");
   	   Assert(X = Result, "Substraction with Big Number 1024 and 1 Bit failed.");

   	   X := X_1024 - X_0;
   	   Assert(X = X_1024, "Substraction with Big Number 1024 and 0 Bit failed.");
   	   
   end Big_Number5_Test4;

--------------------------------------------------------------------------------------
---------------------------------------- Test 5 --------------------------------------
--------------------------------------------------------------------------------------

   procedure Big_Number5_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := X_768 - X_768;
   	   Assert(X = X_0, "Substraction with Big Number 768 Bit failed.");

   	   X := X_768 - X_1;
	   Result := To_Big_Unsigned("10525180923007809127429104552568860171166966111" &
	   "3905203802605095268637688633087840882864647795048773069713107320728346715" &
	   "8004411480234792834567942872750312811392044549722208485350266354690698472" &
	   "582526289123371646877892846653816057849");
   	   Assert(X = Result, "Substraction with Big Number 768 Bit failed.");

   	   X := X_768 - X_0;
   	   Assert(X = X_768, "Substraction with Big Number 768 Bit failed.");

   end Big_Number5_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := X_1 - X_1;
   	   Assert(X = X_0 and X = 0, "Substraction with Big Number 1 Bit failed.");

   	   X := X_1 - X_0;
   	   Assert(X = X_1 and X = 1, "Substraction with Big Number 1 Bit failed.");

   end Big_Number5_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number5_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := X_0 - X_0;
   	   Assert(X = X_0 and X = 0, "Substraction with Big Number 1 Bit failed.");

   end Big_Number5_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Sub;
