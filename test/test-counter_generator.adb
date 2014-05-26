with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Ada.Directories;

package body Test.Counter_Generator is

   procedure Register_Tests (T: in out Test_Counter_Generator) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Counter_Generator_Test1'Access, "Counter Generator");
   end Register_Tests;

   function Name (T: Test_Counter_Generator) return Test_String is
   begin
      return Format ("Counter Generator Test");
   end Name;

   procedure Counter_Generator_Test1 (T : in out Test_Cases.Test_Case'Class) is
      subtype Block is Natural range 0..255;

      function Inc(Item: Block) return Block is
         Result: Block := Item;
      begin
         Result := Result + 1;
         return Result;
      end Inc;

      package N is new Crypto.Types.Nonces(Block => Block);
      package Counter is new N.Nonces_Ctr(Inc => Inc);
      Nonce: Counter.Nonce_Ctr;
      B: Block;
      Result : array(0..99) of Block;
      Distinct : Boolean := true;
   begin
      B := 0;
      Counter.Initialize(This      => Nonce,
                         File_Path => "last_nonce.txt",
                         IV        => B);
      for i in Result'First..(Result'Last/2) loop
         Result(i) := Counter.Update(Nonce);
      end loop;

      Counter.Finalize(Nonce);
      for i in (Result'Last/2+1)..Result'Last loop
         Counter.Initialize(Nonce, "last_nonce.txt");
         Result(i) := Counter.Update(Nonce);
         Counter.Finalize(Nonce);
      end loop;

         for i in Result'Range loop
            for j in (i+1)..Result'Last loop
               if Result(i) = Result(j) then
                  Distinct := false;
               end if;
            end loop;
         end loop;
      Ada.Directories.Delete_File("last_nonce.txt");
      Assert(Distinct,"Counter Generator failed!");
   end Counter_Generator_Test1;

end Test.Counter_Generator;
