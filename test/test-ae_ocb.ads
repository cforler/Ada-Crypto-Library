with AUnit;
with AUnit.Test_Cases;

package Test.AE_OCB is
  use AUnit;

  type OCB_Test is new Test_Cases.Test_Case with null record;

  procedure Register_Tests(T: in out OCB_Test);

  function Name(T: OCB_Test) return Test_String;

  procedure OCB_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.AE_OCB;
