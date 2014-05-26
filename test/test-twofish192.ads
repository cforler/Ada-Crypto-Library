with AUnit;
with AUnit.Test_Cases;

package Test.Twofish192 is
	use AUnit;

	type Twofish_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Twofish_Test);

	function Name(T: Twofish_Test) return Test_String;

	procedure Twofish192_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure Twofish192_Test2(T: in out Test_Cases.Test_Case'Class);

	procedure Twofish192_Test3(T: in out Test_Cases.Test_Case'Class);

end Test.Twofish192;
