with AUnit;
with AUnit.Test_Cases;

package Test.ECDH is
	use AUnit;

	type ECDH_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out ECDH_Test);

	function Name(T: ECDH_Test) return Test_String;

	procedure ECDH_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.ECDH;
