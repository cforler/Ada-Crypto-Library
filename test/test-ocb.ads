with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.OCB is
	use AUnit;
	use Ada.Strings.Unbounded;

	type OCB3_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out OCB3_Test);

	function Name(T: OCB3_Test) return Test_String;

	procedure OCB3_Test_Initialize(T: in out Test_Cases.Test_Case'Class);

	procedure OCB3_Test_Encryption(T: in out Test_Cases.Test_Case'Class);

	procedure OCB3_Test_Exceptions(T: in out Test_Cases.Test_Case'Class);

end Test.OCB;
