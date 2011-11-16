with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Big_Number_Pow_Mod_Utils is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Big_Number_Test is new Test_Cases.Test_Case with null record;

	procedure Results;

	procedure Register_Tests(T: in out Big_Number_Test);

	function Name(T: Big_Number_Test) return Test_String;

	procedure Big_Number_Mod_Utils_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test2(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test3(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test4(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test5(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test6(T: in out Test_Cases.Test_Case'Class);

	procedure Big_Number_Mod_Utils_Test7(T: in out Test_Cases.Test_Case'Class);

end Test.Big_Number_Pow_Mod_Utils;
