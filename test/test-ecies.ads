with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.ECIES is
	use AUnit;
	use Ada.Strings.Unbounded;

	type ECIES_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out ECIES_Test);

	function Name(T: ECIES_Test) return Test_String;

	procedure ECIES_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.ECIES;
