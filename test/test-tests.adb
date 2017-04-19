with Test.Suite_All;
with AUnit.Run;
with Crypto.Types.Random_Source.Provider;
with Crypto.Types.Random;
with AUnit.Reporter.Text;
with AUnit.Test_Results.ACL;

procedure Test.Tests is
   procedure Run_All is new AUnit.Run.Test_Runner_With_Results(Test.Suite_All.Suite);   
   Reporter : AUnit.Reporter.Text.Text_Reporter;   
   Results : AUnit.Test_Results.ACl.ACL_Result;
   
   use Crypto.Types.Random_Source.Provider;
   Dev_U_Rand : Random_Source_Provider;
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Types.Random.Set(Dev_U_Rand);
   Run_All(Reporter, Results); 
end Test.Tests;
