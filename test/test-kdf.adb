with Test.Suite_Key_Derivation_Function;
with AUnit.Run;
with AUnit.Reporter.Text;
with Crypto.Types.Random_Source.Provider;
with Crypto.Types.Random;

procedure Test.KDF is
   procedure Run_KDF is new AUnit.Run.Test_Runner(Test.Suite_Key_Derivation_Function.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   use Crypto.Types.Random_Source.Provider;
   Dev_U_Rand : Random_Source_Provider;
	
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Types.Random.Set(Dev_U_Rand);
   
   Run_KDF(Reporter);
end Test.KDF;
