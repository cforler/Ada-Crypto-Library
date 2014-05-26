with AUnit;
with AUnit.Test_Cases;

package Test.RMAC is
	use AUnit;

	type RMAC_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out RMAC_Test);

	function Name(T: RMAC_Test) return Test_String;

	procedure RMAC_Test1(T: in out Test_Cases.Test_Case'Class);
   	procedure RMAC_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.RMAC;
