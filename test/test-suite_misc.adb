with Test.Types;
with Test.Random;
with Test.Base64;

package body Test.Suite_Misc  is
   Result	 		: aliased Test_Suite;
   Test_Types			: aliased Test.Types.Types_Test;
   Test_Random			: aliased Test.Random.Random_Test;
   Test_Base64			: aliased Test.Base64.Base64_Test;


   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Types'Access);
      Add_Test(Result'Access, Test_Base64'Access);
      --  Add_Test(Result'Access, Test_Random'Access);
      --  Add_Test(Result'Access, Test_MMH'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Misc;
