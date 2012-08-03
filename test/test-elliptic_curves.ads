with AUnit;
with AUnit.Test_Cases;

package Test.Elliptic_curves is
   use AUnit;

   type Elliptic_curves_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Elliptic_curves_Test);

   function Name(T: Elliptic_curves_Test) return Test_String;

   procedure Elliptic_curves_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure Elliptic_curves_Test5(T: in out Test_Cases.Test_Case'Class);
     procedure Elliptic_curves_Put(T: in out Test_Cases.Test_Case'Class);

end Test.Elliptic_curves;
