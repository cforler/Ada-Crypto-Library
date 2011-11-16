with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Test.template is
	use AUnit;
	use Ada.Strings.Unbounded;

	type template_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out template_Test);

	function Name(T: template_Test) return Test_String;

	procedure template_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.template;
