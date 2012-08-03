with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Random;
use Crypto.Types;

package body Test.Random_Generator is

   procedure Register_Tests (T: in out Test_Random_Generator) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Random_Generator_Test1'Access, "Random Generator");
   end Register_Tests;

   function Name (T: Test_Random_Generator) return Test_String is
   begin
      return Format ("Random Generator Test");
   end Name;

   procedure Random_Generator_Test1 (T : in out Test_Cases.Test_Case'Class) is
      package N is new Crypto.Types.Nonces(Crypto.Types.B_Block128);
      package Rand is new N.Nonces_Random(Crypto.Types.To_B_Block128);
      Nonce: Rand.Nonce_Rand;
      Byte_Array: Crypto.Types.B_Block128 := (others => 0);
      Result : array(0..99) of Crypto.Types.B_Block128;
      Distinct : Boolean := true;
   begin
      for i in Result'Range loop
         Result(i) := Rand.Update(Nonce);
      end loop;

         for i in Result'Range loop
            for j in (i+1)..Result'Last loop
               if Result(i) = Result(j) then
                  Distinct := false;
               end if;
               end loop;
         end loop;
      Assert(Distinct,"Random Generator failed!");
   end Random_Generator_Test1;

end Test.Random_Generator;
