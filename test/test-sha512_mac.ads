with AUnit;
with AUnit.Test_Cases;

package Test.SHA512_MAC is
	use AUnit;


	type HMAC_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out HMAC_Test);

	function Name(T: HMAC_Test) return Test_String;

	procedure SHA512_MAC_Test1(T: in out Test_Cases.Test_Case'Class);

	procedure SHA512_MAC_Test2(T: in out Test_Cases.Test_Case'Class);
	
	procedure SHA512_MAC_Test3(T: in out Test_Cases.Test_Case'Class);

end Test.SHA512_MAC;
