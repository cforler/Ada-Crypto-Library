with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.BPS_Mode is
	use AUnit;
	use Ada.Strings.Unbounded;

	type BPS_Mode_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out BPS_Mode_Test);

	function Name(T: BPS_Mode_Test) return Test_String;

        procedure BPS_Mode_Test1(T: in out Test_Cases.Test_Case'Class);

        procedure BPS_Mode_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.BPS_Mode;
