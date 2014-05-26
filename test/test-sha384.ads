with AUnit;
with AUnit.Test_Cases;

package Test.SHA384 is
	use AUnit;

	type SHA_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out SHA_Test);

	function Name(T: SHA_Test) return Test_String;

	procedure SHA384_Test1(T: in out Test_Cases.Test_Case'Class);
	
	procedure SHA384_Test2(T: in out Test_Cases.Test_Case'Class);
	
	procedure SHA384_Test3(T: in out Test_Cases.Test_Case'Class);
	
end Test.SHA384;
