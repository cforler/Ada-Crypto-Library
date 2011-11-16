with AUnit;
with AUnit.Test_Cases;

package Test.ECDSA is
   use AUnit;
   
   type ECDSA_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out ECDSA_Test);
   
   function Name(T: ECDSA_Test) return Test_String;
   
   procedure ECDSA_Test1(T: in out Test_Cases.Test_Case'Class);
   
   procedure ECDSA_Test2(T: in out Test_Cases.Test_Case'Class);

end Test.ECDSA;
