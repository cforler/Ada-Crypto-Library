with Test.Big_Number_XOR;
with Test.Big_Number_OR;
with Test.Big_Number_AND;
with Test.Big_Number_Exponentiate;
with Test.Big_Number_Mod;
with Test.Big_Number_Min_Max;
with Test.Big_Numbers_Utils;
with Test.Big_Number_Inc;
with Test.Big_Number_Dec;


package body Test.Suite_Big_Num3  is
   Result	 		: aliased Test_Suite;
   Test_XOR 			: aliased Test.Big_Number_XOR.Big_Number_Test;
   Test_OR 			: aliased Test.Big_Number_OR.Big_Number_Test;
   Test_AND 			: aliased Test.Big_Number_AND.Big_Number_Test;
   Test_Exponentiate 		: aliased Test.Big_Number_Exponentiate.Big_Number_Test;
   Test_Mod			: aliased Test.Big_Number_Mod.Big_Number_Test;
   Test_Min_Max	        	: aliased Test.Big_Number_Min_Max.Big_Number_Test;
   Test_Utils			: aliased Test.Big_Numbers_Utils.Big_Number_Test;
   Test_Increase		: aliased Test.Big_Number_Inc.Big_Number_Test;
   Test_Decrease		: aliased Test.Big_Number_Dec.Big_Number_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_XOR'Access);
      Add_Test(Result'Access, Test_OR'Access);
      Add_Test(Result'Access, Test_AND'Access);
      Add_Test(Result'Access, Test_Exponentiate'Access);
      Add_Test(Result'Access, Test_Mod'Access);
      Add_Test(Result'Access, Test_Min_Max'Access);
      Add_Test(Result'Access, Test_Utils'Access);
      Add_Test(Result'Access, Test_Increase'Access);
      Add_Test(Result'Access, Test_Decrease'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Big_Num3;
