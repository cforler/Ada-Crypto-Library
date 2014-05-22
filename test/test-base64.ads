with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Base64 is
	use AUnit;
	
	type Base64_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Base64_Test);

	function Name(T: Base64_Test) return Test_String;

	procedure Base64_Rest0_Test(T: in out Test_Cases.Test_Case'Class);
	
	procedure Base64_Rest1_Test(T: in out Test_Cases.Test_Case'Class);
	
	procedure Base64_Rest2_Test(T: in out Test_Cases.Test_Case'Class);
end Test.Base64;
