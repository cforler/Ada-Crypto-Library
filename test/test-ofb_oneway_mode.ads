with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.OFB_Oneway_Mode is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Mode_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Mode_Test);

	function Name(T: Mode_Test) return Test_String;

	procedure OFB_Oneway_Mode_Test1(T: in out Test_Cases.Test_Case'Class);
        procedure OFB_Oneway_Mode_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.OFB_Oneway_Mode;
