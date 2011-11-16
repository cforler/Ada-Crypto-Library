with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Blowfish is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Blowfish128_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Blowfish128_Test);

	function Name(T: Blowfish128_Test) return Test_String;

	procedure Blowfish_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure Blowfish_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.Blowfish;
