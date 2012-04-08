with Test.Suite_Hash_Function;
with Test.Suite_MAC;
with AUnit.Run;
with AUnit.Reporter.Text;
with Crypto.Random_Source.File;
with Crypto.Random;

procedure Test.HASH is
   procedure Run_HASH is new AUnit.Run.Test_Runner(Test.Suite_Hash_Function.Suite);
   procedure Run_MAC  is new AUnit.Run.Test_Runner(Test.Suite_MAC.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   use Crypto.Random_Source.File;
   Dev_U_Rand : Random_Source_File;
	
begin
   Dev_U_Rand.Initialize("/dev/urandom");
   Crypto.Random.Set(Dev_U_Rand);
   
   Run_HASH(Reporter);
   Run_MAC(Reporter);
end Test.HASH;
