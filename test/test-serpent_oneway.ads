with AUnit;
with AUnit.Test_Cases;

package Test.Serpent_Oneway is
   use AUnit;

   type Serpent_Oneway_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Serpent_Oneway_Test);

   function Name(T: Serpent_Oneway_Test) return Test_String;

   procedure Serpent_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure Serpent_Oneway_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.Serpent_Oneway;
