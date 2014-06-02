with Test.DSA;
with Test.RSA;
with Test.ECDSA;

package body Test.Suite_Asymmetric_Ciphers is
   use Aunit.Test_Suites;
   Result:     aliased Test_Suite;
   Test_DSA:   aliased Test.DSA.DSA_Test;
   Test_RSA:   aliased Test.RSA.RSA_Test;
   Test_ECDSA: aliased Test.ECDSA.ECDSA_Test;
   
   function Suite return Aunit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_DSA'Access);
      Add_Test(Result'Access, Test_RSA'Access);
      Add_Test(Result'Access, Test_ECDSA'Access);
      return Result'Access;  
   end Suite;
end Test.Suite_Asymmetric_Ciphers;
