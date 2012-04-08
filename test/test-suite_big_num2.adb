with Test.Big_Number_IsEven;
with Test.Big_Number_IsOdd;
with Test.Big_Number_LSB;
with Test.Big_Number_MSB;
with Test.Big_Number_SR;
with Test.Big_Number_SL;
with Test.Big_Number_RL;
with Test.Big_Number_Swap;
with Test.Big_Number_Add_Mod_Utils;
with Test.Big_Number_Sub_Mod_Utils;


package body Test.Suite_Big_Num2  is
   Result	 		: aliased Test_Suite;
   Test_Is_Even			: aliased Test.Big_Number_IsEven.Big_Number_Test;
   Test_Is_Odd			: aliased Test.Big_Number_IsOdd.Big_Number_Test;
   Test_Least_Significant_Bit	: aliased Test.Big_Number_LSB.Big_Number_Test;
   Test_Most_Significant_Bit	: aliased Test.Big_Number_MSB.Big_Number_Test;
   Test_Shift_Right		: aliased Test.Big_Number_SR.Big_Number_Test;
   Test_Shift_Left	        : aliased Test.Big_Number_SL.Big_Number_Test;
   Test_Rotate_Left		: aliased Test.Big_Number_RL.Big_Number_Test;
   Test_Swap			: aliased Test.Big_Number_Swap.Big_Number_Test;
   Test_Add_Mod_Utils		: aliased Test.Big_Number_Add_Mod_Utils.Big_Number_Test;
   Test_Sub_Mod_Utils		: aliased Test.Big_Number_Sub_Mod_Utils.Big_Number_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Is_Even'Access);
      Add_Test(Result'Access, Test_Is_Odd'Access);
      Add_Test(Result'Access, Test_Least_Significant_Bit'Access);
      Add_Test(Result'Access, Test_Most_Significant_Bit'Access);
      Add_Test(Result'Access, Test_Shift_Right'Access);
      Add_Test(Result'Access, Test_Shift_Left'Access);
      Add_Test(Result'Access, Test_Rotate_Left'Access);
      Add_Test(Result'Access, Test_Swap'Access);
      Add_Test(Result'Access, Test_Add_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Sub_Mod_Utils'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Big_Num2;
