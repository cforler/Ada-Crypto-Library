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
   
   ------------------------------------------------------------------
   
   function Name (T: Test_Counter_Generator) return Test_String is
   begin
      return Format ("Counter Generator Test");
   end Name;
   
   ------------------------------------------------------------------
   
   
   procedure Counter_Generator_Test1 (T : in out Test_Cases.Test_Case'Class) is
      function Inc(Item: Natural) return Natural is
         Result: Natural := Item;
      begin
         Result := Result + 1;
         return Result;
      end Inc;
      
      package N is new Crypto.Types.Nonces(Natural);
      package Counter is new N.Nonces_Ctr(Inc);
      Nonce: Counter.Nonce_Ctr;
      B: Natural := 1;
      Result : array(0..99) of Natural;
      Distinct : Boolean := true;
      Nonce_File : constant String := "./last_nonce.txt";	
   begin
      B := 0;
      Counter.Initialize(This      => Nonce,
                         File_Path => Nonce_File,
                         IV        => B);
      for i in Result'First..(Result'Last/2) loop
         Result(i) := Counter.Update(Nonce);
      end loop;  
      Counter.Finalize(Nonce);
      
      for i in (Result'Last/2+1)..Result'Last-1 loop
         Counter.Initialize(Nonce,  Nonce_File);
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
      Counter.Finalize(Nonce);
      Ada.Directories.Delete_File(Nonce_File);
      
      Assert(Distinct,"Counter Generator failed!");
     end Counter_Generator_Test1;

end Test.Counter_Generator;
