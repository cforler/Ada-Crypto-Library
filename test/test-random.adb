with AUnit.Assertions;
with Crypto.Types.Random;
with Crypto.Types.Random_Source.File;
with Crypto.Types;

package body Test.Random is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register Random Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Random_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Random_Test1'Access,"Random Byte (Single Read)");
      Register_Routine(T, Random_Test2'Access,"Random Word (Single Read)");
      Register_Routine(T, Random_Test3'Access,"Random DWord (Single Read)");
      Register_Routine(T, Random_Test4'Access,"Random Words (Array Read)");
      Register_Routine(T, Random_Test5'Access,"Random DWords (Array Read)");

   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Random Tests ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Random_Test) return Test_String is
   begin
      return new String'("Random Tests");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Random_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      B_Array : Crypto.Types.Bytes(0..256);
      B : Crypto.Types.Byte;
   begin
      for i in B_Array'Range loop
         Crypto.Types.Random.Read(B);
         B_Array(i) := B;
      end loop;

      Assert(True, "Random Read Byte (Single) Failed");
   end Random_Test1;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Random_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      W_Array : Crypto.Types.Words(0..256);
      W : Crypto.Types.Word;
   begin
      for i in W_Array'Range loop
         Crypto.Types.Random.Read(W);
         W_Array(i) := W;
      end loop;

      Assert(True, "Random Read Word (Single) Failed");
   end Random_Test2;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 3 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Random_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Array : Crypto.Types.DWords(0..256);
      DW : Crypto.Types.DWord;
   begin
      for i in DW_Array'Range loop
         Crypto.Types.Random.Read(DW);
         DW_Array(i) := DW;
      end loop;

      Assert(True, "Random Read DWord (Single) Failed");
   end Random_Test3;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 4 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Random_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      W_Array : Crypto.Types.Words(0..256);
   begin
      Crypto.Types.Random.Read(W_Array);

      Assert(True, "Random Read Words (Array) Failed");
   end Random_Test4;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 5 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Random_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Array : Crypto.Types.DWords(0..256);
   begin
      Crypto.Types.Random.Read(DW_Array);

      Assert(True, "Random Read DWords (Array) Failed");
   end Random_Test5;

end Test.Random;
