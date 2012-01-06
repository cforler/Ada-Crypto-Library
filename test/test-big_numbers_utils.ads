with AUnit;
with AUnit.Test_Cases;

package Test.Big_Numbers_Utils is
   use AUnit;
   
   type Big_Number_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Big_Number_Test);

   function Name(T: Big_Number_Test) return Test_String;
   
   procedure Big_Number_Test_Rotate_Left(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Rotate_Right(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Random(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Bit_Length(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_LSB(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Length_In_Bytes(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Gcd(T: in out Test_Cases.Test_Case'Class);
	
   procedure Big_Number_Test_To_Bytes(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_To_Mod_Types(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_To_String(T: in out Test_Cases.Test_Case'Class);
	
   procedure Big_Number_Test_Put(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Big_Div(T: in out Test_Cases.Test_Case'Class);
   
   procedure Big_Number_Test_Short_Div(T: in out Test_Cases.Test_Case'Class);
   
end Test.Big_Numbers_Utils;
