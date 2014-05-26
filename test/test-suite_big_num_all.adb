with Test.Suite_Big_Num1;
with Test.Suite_Big_Num2;
with Test.Suite_Big_Num3;
with Test.Suite_Big_Num4;


package body Test.Suite_Big_Num_All is
   function Suite return Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := New_Suite;
   begin
      Result.Add_Test(Test.Suite_Big_Num1.Suite);
      Result.Add_Test(Test.Suite_Big_Num2.Suite);
      Result.Add_Test(Test.Suite_Big_Num3.Suite);
      Result.Add_Test(Test.Suite_Big_Num4.Suite);
      return Result;
   end Suite;
end Test.Suite_Big_Num_All;
