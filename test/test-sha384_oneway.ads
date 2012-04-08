with AUnit;
with AUnit.Test_Cases;

package Test.SHA384_Oneway is
   use AUnit;

   type SHA384_Oneway_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out SHA384_Oneway_Test);

   function Name(T: SHA384_Oneway_Test) return Test_String;

   procedure SHA384_Oneway_Test1(T: in out Test_Cases.Test_Case'Class);
   procedure SHA384_Oneway_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.SHA384_Oneway;
