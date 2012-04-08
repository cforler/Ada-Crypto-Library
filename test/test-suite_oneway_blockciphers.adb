with Test.CFB_Oneway_Mode;
with Test.CTR_Oneway_Mode;
with Test.OFB_Oneway_Mode;
with Test.SHA256_Oneway;
with Test.SHA384_Oneway;
with Test.SHA512_Oneway;

package body Test.Suite_Oneway_Blockciphers is
   use AUnit.Test_Suites;
   

   Test_CFB_Oneway_Mode: 	aliased Test.CFB_Oneway_Mode.Mode_Test;
   Test_CTR_Oneway_Mode: 	aliased Test.CTR_Oneway_Mode.Mode_Test;
   Test_OFB_Oneway_Mode: 	aliased Test.OFB_Oneway_Mode.Mode_Test;
   Test_SHA256_Oneway:      	aliased Test.SHA256_Oneway.SHA256_Oneway_Test;
   Test_SHA384_Oneway: 		aliased Test.SHA384_Oneway.SHA384_Oneway_Test;
   Test_SHA512_Oneway: 		aliased Test.SHA512_Oneway.SHA512_Oneway_Test;
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_CFB_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_CTR_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_OFB_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_SHA256_Oneway'Access);
      Add_Test(Result'Access, Test_SHA384_Oneway'Access);
      Add_Test(Result'Access, Test_SHA512_Oneway'Access);
      
      return Result'Access;
   end Suite;
end Test.Suite_Oneway_Blockciphers;
