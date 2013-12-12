with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.SHA512Crypt is
	use AUnit;
	use Ada.Strings.Unbounded;

	type SHA512Crypt_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out SHA512Crypt_Test);

	function Name(T: SHA512Crypt_Test) return Test_String;

	procedure SHA512Crypt_Test_Add_Bytes(T: in out Test_Cases.Test_Case'Class);

	procedure SHA512Crypt_Test_Encryption(T: in out Test_Cases.Test_Case'Class);

	procedure SHA512Crypt_Test_Exceptions(T: in out Test_Cases.Test_Case'Class);

end Test.SHA512Crypt;
