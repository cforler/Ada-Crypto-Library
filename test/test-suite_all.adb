with Test.Suite_Asymmetric_Ciphers;
with Test.Suite_Big_Num;
with Test.Suite_Blockciphers;
with Test.Suite_Elliptic_Curves;
with Test.Suite_MAC;
with Test.Suite_SHA;
with AUnit.Tests;


package body Test.Suite_All is  
   function Suite return Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : Access_Test_Suite := New_Suite;
   begin
      Result.Add_Test(Test.Suite_Asymmetric_Ciphers.Suite);
      Result.Add_Test(Test.Suite_Big_Num.Suite);
      Result.Add_Test(Test.Suite_Blockciphers.Suite);
      Result.Add_Test(Test.Suite_Elliptic_Curves.Suite);
      Result.Add_Test(Test.Suite_MAC.Suite);
      Result.Add_Test(Test.Suite_SHA.Suite);
      return Result;
   end Suite;
end Test.Suite_All;
