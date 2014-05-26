with Test.Suite_Asymmetric_Ciphers;
with Test.Suite_Big_Num_All;
with Test.Suite_Blockciphers;
with Test.Suite_Oneway_Blockciphers;
with Test.Suite_Elliptic_Curves;
with Test.Suite_MAC;
with Test.Suite_Hash_Function;
with Test.Suite_Nonces;
with Test.Suite_Misc;
with Test.Suite_Key_Derivation_Function;

package body Test.Suite_All is
   function Suite return Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
      Result : constant Access_Test_Suite := New_Suite;
   begin
      Result.Add_Test(Test.Suite_Asymmetric_Ciphers.Suite);
      Result.Add_Test(Test.Suite_Big_Num_All.Suite);
      Result.Add_Test(Test.Suite_Blockciphers.Suite);
      Result.Add_Test(Test.Suite_Oneway_Blockciphers.Suite);
      Result.Add_Test(Test.Suite_Elliptic_Curves.Suite);
      Result.Add_Test(Test.Suite_MAC.Suite);
      Result.Add_Test(Test.Suite_Hash_Function.Suite);
      Result.Add_Test(Test.Suite_Nonces.Suite);
      Result.Add_Test(Test.Suite_Misc.Suite);
      Result.Add_Test(Test.Suite_Key_Derivation_Function.Suite);
      return Result;
   end Suite;
end Test.Suite_All;
