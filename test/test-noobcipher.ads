with AUnit;
with AUnit.Test_Cases;

package Test.Noobcipher is
   use AUnit;

   type Noobcipher_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Noobcipher_Test);

   function Name(T: Noobcipher_Test) return Test_String;

   procedure Noobcipher_Test1(T: in out Test_Cases.Test_Case'Class);

end Test.Noobcipher;
