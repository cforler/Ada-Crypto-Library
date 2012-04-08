with AUnit;
with AUnit.Test_Cases;

package Test.Big_numbers_division is
   use AUnit;

   type Big_numbers_division_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out Big_numbers_division_Test);

   function Name(T: Big_numbers_division_Test) return Test_String;

   procedure Big_numbers_division_Test1(T: in out Test_Cases.Test_Case'Class);


end Test.Big_numbers_division;
