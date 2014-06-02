with Test.OCB;

package body Test.Suite_AE is
   use AUnit.Test_Suites;
   
   Result:    aliased Test_Suite;
   OCB3_Test: aliased Test.OCB.OCB3_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test(Result'Access, OCB3_Test'Access);
      
      return Result'Access;
   end Suite;
end Test.Suite_AE;
