with AUnit;
use AUnit;
with AUnit.Test_Cases;
use AUnit.Test_Cases;


package Test.Counter_Generator is

   type Test_Counter_Generator is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Test_Counter_Generator);

   function Name (T: Test_Counter_Generator) return Message_String;

   procedure Counter_Generator_Test1 (T : in out Test_Cases.Test_Case'Class);

end Test.Counter_Generator;
