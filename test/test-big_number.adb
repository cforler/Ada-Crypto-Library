with Test.Suite_Big_Num_All;
with AUnit.Run;
with AUnit.Reporter.Text;
with Crypto.Types.Random;
with AUnit.Test_Results.ACL;
with Crypto.Types.Random_Source.File;

procedure Test.Big_Number is
   procedure Run is new AUnit.Run.Test_Runner_With_Results(Test.Suite_Big_Num_All.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Results : AUnit.Test_Results.ACl.ACL_Result;
   
   use Crypto.Types.Random_Source.File;
   Dev_U_Rand : Random_Source_File;
   
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Types.Random.Set(Dev_U_Rand);
   Run(Reporter, Results); 
end Test.Big_Number;
