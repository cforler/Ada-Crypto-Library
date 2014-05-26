with AUnit;
with AUnit.Test_Cases;

package Test.Hmac is
	use AUnit;

	type Hmac_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Hmac_Test);

	function Name(T: Hmac_Test) return Test_String;

        procedure Hmac_Test1(T: in out Test_Cases.Test_Case'Class);

   	procedure Hmac_Test2(T: in out Test_Cases.Test_Case'Class);

   	procedure Hmac_Test3(T: in out Test_Cases.Test_Case'Class);

        procedure Hmac_Test4(T: in out Test_Cases.Test_Case'Class);

        procedure Hmac_Test5(T: in out Test_Cases.Test_Case'Class);

end Test.Hmac;
