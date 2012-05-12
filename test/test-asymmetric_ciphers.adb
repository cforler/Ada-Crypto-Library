with Test.Suite_Asymmetric_Ciphers;
with Test.Suite_Elliptic_Curves;
with AUnit.Run;
with AUnit.Reporter.Text;
with Crypto.Types.Random_Source.File;
with Crypto.Types.Random;


procedure Test.Asymmetric_Ciphers is
   procedure Run_Asymmetric_Ciphers is new AUnit.Run.Test_Runner(Test.Suite_Asymmetric_Ciphers.Suite);
   procedure Run_Elliptic_Curves is new AUnit.Run.Test_Runner(Test.Suite_Elliptic_Curves.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   use Crypto.Types.Random_Source.File;
   Dev_U_Rand : Random_Source_File;
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Types.Random.Set(Dev_U_Rand);

   Run_Asymmetric_Ciphers(Reporter);
   Run_Elliptic_Curves(Reporter);
end Test.Asymmetric_Ciphers;
