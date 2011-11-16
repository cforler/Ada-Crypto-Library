with AUnit;
with AUnit.Test_Cases;

package Test.Big_Numbers_Utils is
   use AUnit;
   
   type Big_Number_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Big_Number_Test);

   function Name(T: Big_Number_Test) return Test_String;
   
   procedure Big_Number_Test10(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test11(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test12(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test13(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test14(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test15(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test16(T: in out Test_Cases.Test_Case'Class);
	
   procedure Big_Number_Test17(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test18(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test19(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test20(T: in out Test_Cases.Test_Case'Class);
	
   procedure Big_Number_Test21(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test22(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test23(T: in out Test_Cases.Test_Case'Class);
   
end Test.Big_Numbers_Utils;
