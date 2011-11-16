with AUnit;
with AUnit.Test_Cases;

package Test.AES128 is
   use AUnit;
   type AES_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out AES_Test);
   
   function Name(T: AES_Test) return Test_String;
   
   procedure AES128_Test1(T: in out Test_Cases.Test_Case'Class);
end Test.AES128;
