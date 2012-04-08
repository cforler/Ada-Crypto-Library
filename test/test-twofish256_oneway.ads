with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Twofish256_Oneway is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Twofish_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Twofish_Test);

	function Name(T: Twofish_Test) return Test_String;

	procedure Twofish256_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);


end Test.Twofish256_Oneway;
