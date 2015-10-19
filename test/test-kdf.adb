with Test.Suite_Key_Derivation_Function;
with AUnit.Reporter.Text;
with AUnit.Run;

procedure Test.KDF is
   procedure Run_KDF is new 
     AUnit.Run.Test_Runner(Test.Suite_Key_Derivation_Function.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_KDF(Reporter);
end Test.KDF;
