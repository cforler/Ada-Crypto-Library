with AUnit;
with AUnit.Test_Cases;

package Test.Types is
   use AUnit;

   type Types_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Types_Test);

   function Name(T: Types_Test) return Test_String;

   procedure Types_Test1(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test2(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test3(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test4(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test5(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test6(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test7(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test8(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test9(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test10(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test11(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test12(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test13(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test14(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test15(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test16(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test17(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test21(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test22(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test23(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test24(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test25(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test26(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test27(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test28(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test29(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test30(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test31(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test32(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test33(T: in out Test_Cases.Test_Case'Class);

   procedure Types_Test34(T: in out Test_Cases.Test_Case'Class);

end Test.Types;
