with AUnit;
with AUnit.Test_Cases;

package Test.Blowfish_Oneway is
   use AUnit;

   type Blowfish_Oneway_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Blowfish_Oneway_Test);

   function Name(T: Blowfish_Oneway_Test) return Test_String;

   procedure Blowfish_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure Blowfish_Oneway_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.Blowfish_Oneway;
