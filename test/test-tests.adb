with Test.Suite_SHA;
with Test.Suite_MAC;
with Test.Suite_Blockciphers;
with Test.Suite_Big_Num;
with Test.Suite_Asymmetric_Ciphers;
with Test.Suite_Elliptic_Curves;
with AUnit.Run;
with AUnit.Reporter.Text;



procedure Test.Tests is

   
   procedure Run_SHA is new AUnit.Run.Test_Runner(Test.Suite_SHA.Suite);
   procedure Run_MAC is new AUnit.Run.Test_Runner(Test.Suite_MAC.Suite);
   procedure Run_Blockciphers is new AUnit.Run.Test_Runner(Test.Suite_Blockciphers.Suite);
   procedure Run_Big_Number is new AUnit.Run.Test_Runner(Test.Suite_Big_Num.Suite);
   procedure Run_Asymmetric_Ciphers is new AUnit.Run.Test_Runner(Test.Suite_Asymmetric_Ciphers.Suite);
   procedure Run_Elliptic_Curves is new AUnit.Run.Test_Runner(Test.Suite_Asymmetric_Ciphers.Suite);
   
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
begin
   Run_SHA(Reporter);
   Run_MAC(Reporter);
   Run_Blockciphers(Reporter);
   Run_Big_Number(Reporter);
   Run_Asymmetric_Ciphers(Reporter);
   Run_Elliptic_Curves(Reporter);
end Test.Tests;
