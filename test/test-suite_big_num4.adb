with Test.Big_Number_Mult_Mod_Utils;
with Test.Big_Number_Inv_Mod_Utils;
with Test.Big_Number_Div_Mod_Utils;
with Test.Big_Number_Pow_Mod_Utils;
with Test.Big_Number_Rand_Mod_Utils;
with Test.Big_Number_Prime_Mod_Utils;
with Test.Big_Number_LPrime_Mod_Utils;
with Test.Big_Number_MR_Mod_Utils;
with Test.Big_Number_B_Add;
with Test.Big_Number_B_Sub;
with Test.Big_Number_B_Mult;
with Test.Big_Number_B_Div;
with Test.Big_numbers_division;


package body Test.Suite_Big_Num4  is
   Result	 		: aliased Test_Suite;
   Test_Mult_Mod_Utils		: aliased Test.Big_Number_Mult_Mod_Utils.Big_Number_Test;
   Test_Inv_Mod_Utils		: aliased Test.Big_Number_Inv_Mod_Utils.Big_Number_Test;
   Test_Div_Mod_Utils		: aliased Test.Big_Number_Div_Mod_Utils.Big_Number_Test;
   Test_Pow_Mod_Utils		: aliased Test.Big_Number_Pow_Mod_Utils.Big_Number_Test;
   Test_Rand_Mod_Utils		: aliased Test.Big_Number_Rand_Mod_Utils.Big_Number_Test;
   Test_Prime_Mod_Utils		: aliased Test.Big_Number_Prime_Mod_Utils.Big_Number_Test;
   Test_LPrime_Mod_Utils	: aliased Test.Big_Number_LPrime_Mod_Utils.Big_Number_Test;
   Test_MR_Mod_Utils		: aliased Test.Big_Number_MR_Mod_Utils.Big_Number_Test;
   Test_B_Add			: aliased Test.Big_Number_B_Add.Big_Number_Test;
   Test_B_Sub			: aliased Test.Big_Number_B_Sub.Big_Number_Test;
   Test_B_Mult			: aliased Test.Big_Number_B_Mult.Big_Number_Test;
   Test_B_Div			: aliased Test.Big_Number_B_Div.Big_Number_Test;
   Test_Big_numbers_division	: aliased Test.Big_numbers_division.Big_numbers_division_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Mult_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Inv_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Div_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Pow_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Rand_Mod_Utils'Access);
      Add_Test(Result'Access, Test_Prime_Mod_Utils'Access);
      Add_Test(Result'Access, Test_LPrime_Mod_Utils'Access);
      Add_Test(Result'Access, Test_MR_Mod_Utils'Access);
      Add_Test(Result'Access, Test_B_Add'Access);
      Add_Test(Result'Access, Test_B_Sub'Access);
      Add_Test(Result'Access, Test_B_Mult'Access);
      Add_Test(Result'Access, Test_B_Div'Access);
      Add_Test(Result'Access, Test_Big_numbers_division'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Big_Num4;
