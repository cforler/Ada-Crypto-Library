with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Serpent is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Serpent_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Serpent_Test);

	function Name(T: Serpent_Test) return Test_String;

	procedure Serpent_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure Serpent_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.Serpent;
