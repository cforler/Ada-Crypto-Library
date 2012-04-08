with AUnit;
with AUnit.Test_Cases;

package Test.SHA256_Oneway is
   use AUnit;

   type SHA256_Oneway_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out SHA256_Oneway_Test);

   function Name(T: SHA256_Oneway_Test) return Test_String;

   procedure SHA256_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure SHA256_Oneway_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.SHA256_Oneway;
