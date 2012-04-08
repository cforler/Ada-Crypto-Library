with AUnit;
with AUnit.Test_Cases;

package Test.Aes_Oneway is
   use AUnit;

   type Aes_Oneway_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Aes_Oneway_Test);

   function Name(T: Aes_Oneway_Test) return Test_String;

   procedure Aes_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure Aes_Oneway_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.Aes_Oneway;
