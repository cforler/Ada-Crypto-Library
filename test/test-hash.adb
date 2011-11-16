with Test.Suite_SHA;
with Test.Suite_MAC;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Test.HASH is
	procedure Run_HASH is new AUnit.Run.Test_Runner(Test.Suite_SHA.Suite);
	procedure Run_MAC  is new AUnit.Run.Test_Runner(Test.Suite_MAC.Suite);
	Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_HASH(Reporter);
   Run_MAC(Reporter);
end Test.HASH;
