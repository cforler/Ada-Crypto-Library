with AUnit;
with AUnit.Test_Cases;
with Crypto.Types.Base64;

package Test.Base64 is
	use AUnit;

   type Base64_Character is
     ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
      'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b',
      'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
      'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3',
     '4', '5', '6', '7', '8' ,'9', '+', '/', '=');
   
   package Base64 is new Crypto.Types.Base64
     (Base64_Character => Base64_Character);
   
	type Base64_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Base64_Test);

	function Name(T: Base64_Test) return Test_String;

	procedure Base64_Rest0_Test(T: in out Test_Cases.Test_Case'Class);
	
	procedure Base64_Rest1_Test(T: in out Test_Cases.Test_Case'Class);
	
	procedure Base64_Rest2_Test(T: in out Test_Cases.Test_Case'Class);
end Test.Base64;
