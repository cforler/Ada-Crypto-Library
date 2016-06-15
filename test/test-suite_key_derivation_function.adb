with Test.SHA512Crypt;
with Test.PBKDF2;
with Test.Scrypt;
with Test.Catena;

package body Test.Suite_Key_Derivation_Function is
   use AUnit.Test_Suites;

   Result:		aliased Test_Suite;
   SHA512Crypt_Test:	aliased Test.SHA512Crypt.SHA512Crypt_Test;
   PBKDF2_Test:		aliased Test.PBKDF2.PBKDF2_Test;
   Scrypt_Test:		aliased Test.Scrypt.Scrypt_Test;
   Catena_Test:      aliased Test.Catena.Catena_Test;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, Scrypt_Test'Access);
      Add_Test(Result'Access, SHA512Crypt_Test'Access);
      Add_Test(Result'Access, PBKDF2_Test'Access);
      Add_Test(Result'Access, Catena_Test'Access);

      
      return Result'Access;
   end Suite;
end Test.Suite_Key_Derivation_Function;
