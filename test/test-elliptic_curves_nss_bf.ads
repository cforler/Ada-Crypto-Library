with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Elliptic_Curves_NSS_BF is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Elliptic_Curves_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Elliptic_Curves_Test);

	function Name(T: Elliptic_Curves_Test) return Test_String;

	procedure Elliptic_Curves_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test2(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test3(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test4(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test5(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test6(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test7(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test8(T: in out Test_Cases.Test_Case'Class);

	procedure Elliptic_Curves_Test9(T: in out Test_Cases.Test_Case'Class);

end Test.Elliptic_Curves_NSS_BF;
