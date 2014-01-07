with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.Scrypt is
	use AUnit;
	use Ada.Strings.Unbounded;

	type Scrypt_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Scrypt_Test);

	function Name(T: Scrypt_Test) return Test_String;

   	procedure Scrypt_Test_Salsa(T: in out Test_Cases.Test_Case'Class);
   	procedure Scrypt_Test_Block_Mix(T: in out Test_Cases.Test_Case'Class);

end Test.Scrypt;
