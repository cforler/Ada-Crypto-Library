with Test.Suite_Blockciphers;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Test.Symmetric_Ciphers is

   procedure Run_Blockciphers is new AUnit.Run.Test_Runner(Test.Suite_Blockciphers.Suite);
      Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_Blockciphers(Reporter);
end Test.Symmetric_Ciphers;
