with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.SHA256_MAC is
	use AUnit;
	use Ada.Strings.Unbounded;

	type HMAC_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out HMAC_Test);

	function Name(T: HMAC_Test) return Test_String;

	procedure SHA256_MAC_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure SHA256_MAC_Test2(T: in out Test_Cases.Test_Case'Class);
	
	procedure SHA256_MAC_Test3(T: in out Test_Cases.Test_Case'Class);

end Test.SHA256_MAC;
