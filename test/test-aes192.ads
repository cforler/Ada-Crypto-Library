with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.AES192 is
	use AUnit;
	use Ada.Strings.Unbounded;

	type AES_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out AES_Test);

	function Name(T: AES_Test) return Test_String;

	procedure AES192_Test1(T: in out Test_Cases.Test_Case'Class);
	
end Test.AES192;
