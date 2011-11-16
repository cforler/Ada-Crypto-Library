with Test.Suite_Big_Num;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Test.Big_Number is
   procedure Run is new AUnit.Run.Test_Runner (Test.Suite_Big_Num.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run(Reporter);
end Test.Big_Number;
