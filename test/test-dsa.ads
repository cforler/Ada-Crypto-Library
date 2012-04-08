with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.DSA is
	use AUnit;
	use Ada.Strings.Unbounded;

	type DSA_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out DSA_Test);

	function Name(T: DSA_Test) return Test_String;

	procedure DSA_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure DSA_Test2(T: in out Test_Cases.Test_Case'Class);

   	procedure DSA_Test3(T: in out Test_Cases.Test_Case'Class);

        procedure DSA_Test4(T: in out Test_Cases.Test_Case'Class);

   	procedure DSA_Test5(T: in out Test_Cases.Test_Case'Class);

        procedure DSA_Test6(T: in out Test_Cases.Test_Case'Class);

   	procedure DSA_Test7(T: in out Test_Cases.Test_Case'Class);

end Test.DSA;
