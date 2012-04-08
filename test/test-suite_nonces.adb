with Test.Counter_Generator;
with Test.Random_Generator;

package body Test.Suite_Nonces  is
   Result	 		: aliased Test_Suite;
   Test_Counter_Generator	: aliased Test.Counter_Generator.Test_Counter_Generator;
   Test_Random_Generator	: aliased Test.Random_Generator.Test_Random_Generator;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(Result'Access, Test_Counter_Generator'Access);
      Add_Test(Result'Access, Test_Random_Generator'Access);
      return Result'Access;
   end Suite;

end Test.Suite_Nonces;
