with Test.SHA1_MAC;
with Test.SHA256_MAC;
with Test.SHA512_MAC;
with Test.Whirlpool_MAC;
with Test.HMAC;
with Test.Symmetric_Mac;
with Test.RMAC;

package body Test.Suite_MAC is
   use AUnit.Test_Suites;

   Result             : aliased Test_Suite;
   SHA1_MAC_Test      : aliased Test.SHA1_MAC.HMAC_Test;
   SHA256_MAC_Test    : aliased Test.SHA256_MAC.HMAC_Test;
   SHA512_MAC_Test    : aliased Test.SHA512_MAC.HMAC_Test;
   Whirlpool_MAC_Test : aliased Test.Whirlpool_MAC.HMAC_Test;
   Test_HMAC	      : aliased Test.HMAC.HMAC_Test;
   Test_RMAC: 		aliased Test.RMAC.RMAC_Test;
   Test_Symmetric_Mac : aliased Test.Symmetric_Mac.Mac_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, SHA1_MAC_Test'Access);
      Add_Test(Result'Access, SHA256_MAC_Test'Access);
      Add_Test(Result'Access, SHA512_MAC_Test'Access);
      Add_Test(Result'Access, Whirlpool_MAC_Test'Access);
      Add_Test(Result'Access, Test_HMAC'Access);
      Add_Test(Result'Access, Test_RMAC'Access);
      Add_Test(Result'Access, Test_Symmetric_Mac'Access);
      
      return Result'Access;
   end Suite;
end Test.Suite_MAC;
