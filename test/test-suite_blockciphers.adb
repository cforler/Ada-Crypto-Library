with Test.AES128;
with Test.AES192;
with Test.AES256;
with Test.Serpent;
with Test.Blowfish;
with Test.CBC_Mode;
with Test.CFB_Mode;
with Test.CTR_Mode;
with Test.OFB_Mode;
with Test.Twofish128;
with Test.Twofish192;
with Test.Twofish256;
with Test.TDES;
with Test.TDES_Obsolete;
with Test.Noobcipher;

package body Test.Suite_Blockciphers is
   use AUnit.Test_Suites;
   
   Result:		aliased Test_Suite;
   Test_AES128:		aliased Test.AES128.AES_Test;
   Test_AES192: 	aliased Test.AES192.AES_Test;
   Test_AES256: 	aliased Test.AES256.AES_Test;
   Test_Serpent: 	aliased Test.Serpent.Serpent_Test;
   Test_Blowfish: 	aliased Test.Blowfish.Blowfish128_Test;
   Test_CBC_Mode: 	aliased Test.CBC_Mode.Mode_Test;
   Test_CFB_Mode: 	aliased Test.CFB_Mode.Mode_Test;
   Test_CTR_Mode: 	aliased Test.CTR_Mode.Mode_Test;
   Test_OFB_Mode: 	aliased Test.OFB_Mode.Mode_Test;
   Test_Twofish128:	aliased Test.Twofish128.Twofish_Test;
   Test_Twofish192:	aliased Test.Twofish192.Twofish_Test;
   Test_Twofish256:	aliased Test.Twofish256.Twofish_Test;
   Test_TDES:		aliased Test.TDES.TDES_Test;
   Test_TDES_Obsolete:	aliased Test.TDES_Obsolete.TDES_Test;
   Test_Noobcipher:     aliased Test.Noobcipher.Noobcipher_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_AES128'Access);
      Add_Test(Result'Access, Test_AES192'Access);
      Add_Test(Result'Access, Test_AES256'Access);
      Add_Test(Result'Access, Test_Serpent'Access);
      Add_Test(Result'Access, Test_Blowfish'Access);
      Add_Test(Result'Access, Test_CBC_Mode'Access);
      Add_Test(Result'Access, Test_CFB_Mode'Access);
      Add_Test(Result'Access, Test_CTR_Mode'Access);
      Add_Test(Result'Access, Test_OFB_Mode'Access);
      Add_Test(Result'Access, Test_Twofish128'Access);
      Add_Test(Result'Access, Test_Twofish192'Access);
      Add_Test(Result'Access, Test_Twofish256'Access);
      Add_Test(Result'Access, Test_TDES'Access);
      Add_Test(Result'Access, Test_TDES_Obsolete'Access);
      Add_Test(Result'Access, Test_Noobcipher'Access);

      return Result'Access;
   end Suite;
end Test.Suite_Blockciphers;
