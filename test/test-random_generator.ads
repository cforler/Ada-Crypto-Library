with AUnit;
use AUnit;
with AUnit.Test_Cases;
use AUnit.Test_Cases;


package Test.Random_Generator is

   type Test_Random_Generator is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Test_Random_Generator);

   function Name (T: Test_Random_Generator) return Message_String;

   procedure Random_Generator_Test1 (T : in out Test_Cases.Test_Case'Class);

end Test.Random_Generator;
