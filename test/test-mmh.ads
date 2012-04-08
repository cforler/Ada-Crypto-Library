with AUnit;
with AUnit.Test_Cases;

package Test.MMH is
   use AUnit;

   type MMH_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out MMH_Test);

   function Name(T: MMH_Test) return Test_String;

   procedure MMH_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure MMH_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.MMH;
