with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Symmetric_Mac is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Mac_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Mac_Test);

	function Name(T: Mac_Test) return Test_String;

        procedure Mac_Test1(T: in out Test_Cases.Test_Case'Class);

   	procedure Mac_Test2(T: in out Test_Cases.Test_Case'Class);

   	procedure Mac_Test3(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test4(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test5(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test6(T: in out Test_Cases.Test_Case'Class);

   	procedure Mac_Test7(T: in out Test_Cases.Test_Case'Class);

   	procedure Mac_Test8(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test9(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test10(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test11(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test12(T: in out Test_Cases.Test_Case'Class);
	
        procedure Mac_Test20(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test21(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test22(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test23(T: in out Test_Cases.Test_Case'Class);

        procedure Mac_Test24(T: in out Test_Cases.Test_Case'Class);



end Test.Symmetric_Mac;
