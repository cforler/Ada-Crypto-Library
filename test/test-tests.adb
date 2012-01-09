with Test.Suite_All;
with AUnit.Run;
with Crypto.Random_Source.File;
with Crypto.Random;
with AUnit.Reporter.Text;
procedure Test.Tests is
   
   procedure Run_All is new AUnit.Run.Test_Runner(Test.Suite_All.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   use Crypto.Random_Source.File;
   Dev_U_Rand : Random_Source_File;
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Random.Set(Dev_U_Rand);
   Run_All(Reporter); _U_Rand);end Test.Tests;
