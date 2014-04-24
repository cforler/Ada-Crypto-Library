with Test.SHA1;
with Test.SHA256;
with Test.SHA384;
with Test.SHA512;
with Test.Whirlpool;
with Test.Skein;
with Test.OCB;
with Test.SHA512Crypt;
with Test.PBKDF2;
with Test.Scrypt;
with Test.Hmac;

package body Test.Suite_Authenticated_Encryption is
   use AUnit.Test_Suites;

   Result:				aliased Test_Suite;
   OCB_Test:				aliased Test.OCB.OCB3_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, OCB_Test'Access);
      return Result'Access;
   end Suite;
end Test.Suite_Authenticated_Encryption;
