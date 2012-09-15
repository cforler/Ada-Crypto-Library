with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.CMAC is
	use AUnit;
	use Ada.Strings.Unbounded;

	type CMAC_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out CMAC_Test);

	function Name(T: CMAC_Test) return Test_String;

	procedure CMAC_Test1(T: in out Test_Cases.Test_Case'Class);

   	procedure CMAC_Test2(T: in out Test_Cases.Test_Case'Class);

        procedure CMAC_Test3(T: in out Test_Cases.Test_Case'Class);

        procedure CMAC_Test4(T: in out Test_Cases.Test_Case'Class);

        procedure CMAC_Test5(T: in out Test_Cases.Test_Case'Class);

   	procedure CMAC_Test6(T: in out Test_Cases.Test_Case'Class);

        procedure CMAC_Test7(T: in out Test_Cases.Test_Case'Class);

        procedure CMAC_Test8(T: in out Test_Cases.Test_Case'Class);

end Test.CMAC;
