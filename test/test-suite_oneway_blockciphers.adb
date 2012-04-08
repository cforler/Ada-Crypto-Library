with Test.Aes_Oneway;
with Test.Serpent_Oneway;
with Test.Aes_Oneway256;
with Test.Blowfish_Oneway;
with Test.TDES_Oneway;
with Test.Twofish128_Oneway;
with Test.Twofish192_Oneway;
with Test.Twofish256_Oneway;
with Test.Whirlpool_Oneway;

with Test.CFB_Oneway_Mode;
with Test.CTR_Oneway_Mode;
with Test.OFB_Oneway_Mode;
with Test.SHA256_Oneway;
with Test.SHA384_Oneway;
with Test.SHA512_Oneway;

package body Test.Suite_Oneway_Blockciphers is
   use AUnit.Test_Suites;
   
   Test_Aes_Oneway:             aliased Test.Aes_Oneway.Aes_Oneway_Test;
   Test_Serpent_Oneway:         aliased Test.Serpent_Oneway.Serpent_Oneway_Test;
   Test_Aes_Oneway256:          aliased Test.Aes_Oneway256.Aes_Oneway_Test256;
   Test_Blowfish_Oneway:        aliased Test.Blowfish_Oneway.Blowfish_Oneway_Test;
   Test_TDES_Oneway:            aliased Test.TDES_Oneway.TDES_Oneway_Test;
   Test_Twofish128_Oneway:      aliased Test.Twofish128_Oneway.Twofish_Test;
   Test_Twofish192_Oneway:      aliased Test.Twofish192_Oneway.Twofish_Test;
   Test_Twofish256_Oneway:      aliased Test.Twofish256_Oneway.Twofish_Test;
   Test_Whirlpool_Oneway:       aliased Test.Whirlpool_Oneway.Whirlpool_Oneway_Test;

   Test_CFB_Oneway_Mode: 	aliased Test.CFB_Oneway_Mode.Mode_Test;
   Test_CTR_Oneway_Mode: 	aliased Test.CTR_Oneway_Mode.Mode_Test;
   Test_OFB_Oneway_Mode: 	aliased Test.OFB_Oneway_Mode.Mode_Test;
   Test_SHA256_Oneway:      	aliased Test.SHA256_Oneway.SHA256_Oneway_Test;
   Test_SHA384_Oneway: 		aliased Test.SHA384_Oneway.SHA384_Oneway_Test;
   Test_SHA512_Oneway: 		aliased Test.SHA512_Oneway.SHA512_Oneway_Test;
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Aes_Oneway'Access);
      Add_Test(Result'Access, Test_Serpent_Oneway'Access);
      Add_Test(Result'Access, Test_Aes_Oneway256'Access);
      Add_Test(Result'Access, Test_Blowfish_Oneway'Access);
      Add_Test(Result'Access, Test_TDES_Oneway'Access);
      Add_Test(Result'Access, Test_Twofish128_Oneway'Access);
      Add_Test(Result'Access, Test_Twofish192_Oneway'Access);
      Add_Test(Result'Access, Test_Twofish256_Oneway'Access);
      Add_Test(Result'Access, Test_Whirlpool_Oneway'Access);

      Add_Test(Result'Access, Test_CFB_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_CTR_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_OFB_Oneway_Mode'Access);
      Add_Test(Result'Access, Test_SHA256_Oneway'Access);
      Add_Test(Result'Access, Test_SHA384_Oneway'Access);
      Add_Test(Result'Access, Test_SHA512_Oneway'Access);
      
      return Result'Access;
   end Suite;
end Test.Suite_Oneway_Blockciphers;
