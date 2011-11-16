with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.TDES_Obsolete is
	use AUnit;
	use Ada.Strings.Unbounded;

	type TDES_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out TDES_Test);

	function Name(T: TDES_Test) return Test_String;

	procedure TDES_Obsolete_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure TDES_Obsolete_Test2(T: in out Test_Cases.Test_Case'Class);

	procedure TDES_Obsolete_Test3(T: in out Test_Cases.Test_Case'Class);

end Test.TDES_Obsolete;
