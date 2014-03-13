with AUnit;
with AUnit.Test_Cases;
with Ada.Strings.Unbounded;
with Crypto.Debug_Put;

package Test.Skein is
   use AUnit;
   use Ada.Strings.Unbounded;

   type Skein_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Skein_Test);

   function Name (T : Skein_Test) return Test_String;

   package Error_Output is new Crypto.Debug_Put(b => false);

   procedure Skein_Test_256(T: in out Test_Cases.Test_Case'Class);
   procedure Skein_Test_512(T: in out Test_Cases.Test_Case'Class);
   procedure Skein_Test_1024(T: in out Test_Cases.Test_Case'Class);

end Test.Skein;
