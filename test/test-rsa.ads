with AUnit;
with AUnit.Test_Cases;

package Test.RSA is
   use AUnit;

   type RSA_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out RSA_Test);

   function Name(T: RSA_Test) return Test_String;

   procedure RSA_Test1(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test2(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test3(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test4(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test5(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test6(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test7(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test8(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test9(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test10(T: in out Test_Cases.Test_Case'Class);

   procedure RSA_Test11(T: in out Test_Cases.Test_Case'Class);

end Test.RSA;
