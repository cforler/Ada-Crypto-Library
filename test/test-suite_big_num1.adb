with Test.Big_Number_Multiplication;
with Test.Big_Number_Mult2;
with Test.Big_Number_Mod_Type_Comp_and_Add;
with Test.Big_Number_Mod_Types;
with Test.Big_Number_Comp;
with Test.Big_Number_Add;
with Test.Big_Number_Sub;
with Test.Big_Number_Div;


package body Test.Suite_Big_Num1  is
   Result	 		: aliased Test_Suite;
   Test_Mult 			: aliased Test.Big_Number_Multiplication.Big_Number_Test;
   Test_Mult2	 	   	: aliased Test.Big_Number_Mult2.Big_Number_Test;
   Test_Mod_Type_Comp_and_Add	: aliased Test.Big_Number_Mod_Type_Comp_and_Add.Big_Number_Test;
   Test_Mod_Types		: aliased Test.Big_Number_Mod_Types.Big_Number_Test;
   Test_Comp 			: aliased Test.Big_Number_Comp.Big_Number_Test;
   Test_Add 			: aliased Test.Big_Number_Add.Big_Number_Test;
   Test_Sub 			: aliased Test.Big_Number_Sub.Big_Number_Test;
   Test_Div 			: aliased Test.Big_Number_Div.Big_Number_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Mult'Access);
      Add_Test(Result'Access, Test_Mult2'Access);
      Add_Test(Result'Access, Test_Mod_Type_Comp_and_Add'Access);
      Add_Test(Result'Access, Test_Mod_Types'Access);
      Add_Test(Result'Access, Test_Comp'Access);
      Add_Test(Result'Access, Test_Add'Access);
      Add_Test(Result'Access, Test_Sub'Access);
      Add_Test(Result'Access, Test_Div'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Big_Num1;
