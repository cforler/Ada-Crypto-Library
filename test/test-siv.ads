with AUnit;
with AUnit.Test_Cases;

package Test.SIV is
  use AUnit;

  type SIV_Test is new Test_Cases.Test_Case with null record;

  procedure Register_Tests(T: in out SIV_Test);

  function Name(T: SIV_Test) return Test_String;

  procedure SIV_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.SIV;
