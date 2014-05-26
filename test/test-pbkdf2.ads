with AUnit;
with AUnit.Test_Cases;

package Test.PBKDF2 is
	use AUnit;

	type PBKDF2_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out PBKDF2_Test);

	function Name(T: PBKDF2_Test) return Test_String;

	procedure PBKDF2_Test_One(T: in out Test_Cases.Test_Case'Class);

end Test.PBKDF2;
