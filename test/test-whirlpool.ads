with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Whirlpool is
	use AUnit;
	use Ada.Strings.Unbounded;

	type SHA_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out SHA_Test);

	function Name(T: SHA_Test) return Test_String;

	procedure Whirlpool_Test1(T: in out Test_Cases.Test_Case'Class);
	
	procedure Whirlpool_Test2(T: in out Test_Cases.Test_Case'Class);
	
	procedure Whirlpool_Test3(T: in out Test_Cases.Test_Case'Class);

end Test.Whirlpool;
