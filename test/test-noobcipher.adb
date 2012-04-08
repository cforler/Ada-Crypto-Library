with AUnit.Assertions;
with Crypto.Types;
with Crypto.Symmetric.Blockcipher_Noob64;

package body Test.Noobcipher is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register Random Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Noobcipher_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Noobcipher_Test1'Access,"Noobcipher");

   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Random Tests ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Noobcipher_Test) return Test_String is
   begin
      return new String'("Noobcipher Tests");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Noobcipher_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Types;
      Key : Crypto.Types.B_Block64 := (others => 0);
      Plaintext : Crypto.Types.B_Block64 := (others => 1);
      Plaintext2 : Crypto.Types.B_Block64;
      Ciphertext : Crypto.Types.B_Block64;
   begin
      Crypto.Symmetric.Blockcipher_Noob64.Prepare_Key(Key);
      Crypto.Symmetric.Blockcipher_Noob64.Encrypt(Plaintext, Ciphertext);
      Crypto.Symmetric.Blockcipher_Noob64.Decrypt(Ciphertext, Plaintext2);

      Assert(Plaintext = Plaintext2, "Noobcipher Failed");
   end Noobcipher_Test1;

end Test.Noobcipher;
