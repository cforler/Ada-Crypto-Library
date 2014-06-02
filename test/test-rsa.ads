with AUnit;
with AUnit.Test_Cases;

package Test.RSA is
   use AUnit;

   type RSA_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out RSA_Test);

   function Name(T: RSA_Test) return Test_String;

   procedure Test_Verify_Key_Pair(T: in out Test_Cases.Test_Case'Class);
   procedure Test_OAEP(T: in out Test_Cases.Test_Case'Class);
   procedure Test_Gen_Keypair(T: in out Test_Cases.Test_Case'Class);
   procedure Test_Get_Public_Key(T: in out Test_Cases.Test_Case'Class);
   procedure Test_Encryption(T: in out Test_Cases.Test_Case'Class);
   procedure Test_Zero_Encryption(T: in out Test_Cases.Test_Case'Class);
   procedure Test_Get_Private_Key(T: in out Test_Cases.Test_Case'Class);
end Test.RSA;
