with AUnit;
with AUnit.Test_Cases;

package Test.Random is
   use AUnit;

   type Random_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Random_Test);

   function Name(T: Random_Test) return Test_String;

   procedure Random_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure Random_Test2(T: in out Test_Cases.Test_Case'Class);
   procedure Random_Test3(T: in out Test_Cases.Test_Case'Class);
   procedure Random_Test4(T: in out Test_Cases.Test_Case'Class);
   procedure Random_Test5(T: in out Test_Cases.Test_Case'Class);

end Test.Random;
