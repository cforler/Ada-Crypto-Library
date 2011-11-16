with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_SL is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    
	use Big_Number_Constants;
	
	X_4096, X_4095, X_1025, X_1024, X_768, X_1, X_0, P, Q, R, Z : 
	Big_Unsigned;

------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
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

		Register_Routine(T, Big_Number_Test1'Access,"Shift Left");
		Register_Routine(T, Big_Number_Test2'Access,"Shift Left");
		Register_Routine(T, Big_Number_Test3'Access,"Shift Left");
		Register_Routine(T, Big_Number_Test4'Access,"Shift Left");
		Register_Routine(T, Big_Number_Test5'Access,"Shift Left");

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
	   P := Shift_Left(X_1,3);
	   Assert(P = 8, "Failed with 1 Bit shifted left 3 times.");
	   
	   P := Shift_Left(P,3);
	   Assert(P = 64, "Failed with 4 Bit shifted left 3 times.");

	   P := Shift_Left(P,6); 
	   Assert(P = 4096, "Failed with 7 Bit shifted left 6 times.");

	   P := Shift_Left(P,4083); 
	   Assert(P = X_4095, "Failed with 13 Bit shifted left 4083 times.");

   end Big_Number_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   Q := To_Big_Unsigned("2#1000010001#");
   	   P := Shift_Left(Q,2);
   	   Assert(P = 2116, "Failed with 9 Bit shifted 2 times.");
   
   	   P := Shift_Left(P,1024);
	   Q := To_Big_Unsigned("38039186733686604607552097837095763363356392874419207" &
	   "07905780517297623420044400379888111380142143462206901609698078683448059991" &
	   "50811305573194865163233127247183356862487015366844655496676236735156762723" &
	   "52661187220686152684383665145719880014190439043383764968060858460707572837" &
	   "2405823079701031613993484858274349056");
   	   Assert(P = Q, "Failed with 9 Bit shifted 2 times.");
   
   end Big_Number_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
   	   P := Shift_Left(X_1025,167);
	   Q := To_Big_Unsigned("33629842688253419175912847062602803678867040518454472" &
	   "50338522815703269666258153441288571198678915268219994320123429997614957993" &
	   "61148762910644367380522045260637880927180781071892278904228907326638798184" &
	   "85193469777795597041539532361738071433136792903485810905811991081989676911" &
	   "30426308863436961088374540600499216154833256125564888186758851350475764357" &
	   "6447336448");
   	   Assert(P = Q, "Failed with 1025 Bit shifted 167 times.");
   
   end Big_Number_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   P := Shift_Left(X_1024,1);
	   Q := To_Big_Unsigned("35953862697246318154586103815780494672359539578846131" &
	   "45468601623154653516110019262654169546448150720422402277597427867153175795" &
	   "37628833244985694861278948248755535786849730970552604439202492188238906165" &
	   "90417001153767630136468492576294782622108165447432670102136917259647989449" &
	   "1876959432609670712659248448274430");
   	   Assert(P = Q, "Failed with 1024 Bit shifted 1 time.");
   
   end Big_Number_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   P := Shift_Left(X_0,1000);
   	   Assert(P = X_0, "Failed with 0 Bit shifted 1000 times.");

   	   P := Shift_Left(X_1,4095);
   	   Assert(P = X_4095, "Failed with 1 Bit shifted 4095 times.");

   end Big_Number_Test5;

------------------------------------------------------------------------------------

end Test.Big_Number_SL;
