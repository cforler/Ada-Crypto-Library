with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.PBKDF2 is
	use AUnit;
	use Ada.Strings.Unbounded;

	type PBKDF2_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out PBKDF2_Test);

	function Name(T: PBKDF2_Test) return Test_String;

	procedure PBKDF2_Test_Add_Bytes(T: in out Test_Cases.Test_Case'Class);

	procedure PBKDF2_Test_Encryption(T: in out Test_Cases.Test_Case'Class);

	procedure PBKDF2_Test_Exceptions(T: in out Test_Cases.Test_Case'Class);

end Test.PBKDF2;
