with AUnit.Assertions;
with Crypto.Symmetric.Mode.BPS;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;
with Crypto.Random;

pragma Elaborate_All(Crypto.Types);
pragma Elaborate_All(Crypto.Symmetric.Mode.BPS);

package body Test.BPS_Mode is
   use Crypto.Types;
   -----------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------- Type - Declaration --------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

   package AES128 renames Crypto.Symmetric.Blockcipher_AES128;
   use Crypto.Symmetric.Mode;
   package BPS is new Crypto.Symmetric.Mode.BPS(AES128);
   use BPS;

   BPS_Key : B_Block128;
   BPS_IV : B_Block64;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------------------- Register BPS_Mode Test  ---------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

    procedure Register_Tests(T : in out BPS_Mode_Test) is
       use Test_Cases.Registration;
    begin
      Register_Routine(T, BPS_Mode_Test1'Access,"BPS_Mode Test1.");
      Register_Routine(T, BPS_Mode_Test2'Access,"BPS_Mode Test2.");
    end Register_Tests;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------------------ Name BPS_Mode Test ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

   function Name(T : BPS_Mode_Test) return Test_String is
   begin
      return new String'("BPS_Mode Test");
   end Name;

--------------------------------------------------------------------------------
---------------------------------- Start Tests ---------------------------------
--------------------------------------------------------------------------------
------------------------------------ Test 1 ------------------------------------
--------------------------------------------------------------------------------

   procedure BPS_Mode_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      BPS_Plaintext : BPS.Numerals(1..5) := (1,0,0,0,1);
      BPS_Ciphertext, P : BPS.Numerals(1..5);
   begin
      for i in B_Block128'Range loop
         Crypto.Random.Read(BPS_Key(i));
      end loop;
      for j in B_Block64'Range loop
         Crypto.Random.Read(BPS_IV(j));
      end loop;

      Init(BPS_Key, BPS_IV);
      Encrypt(BPS_Plaintext, BPS_Ciphertext);
      Decrypt(BPS_Ciphertext, P);

      Assert(P = BPS_Plaintext, "BPS Mode failed.");

   end BPS_Mode_Test1;

------------------------------------------------------------------------------------
------------------------------------ Test 2 ------------------------------------
--------------------------------------------------------------------------------

   procedure BPS_Mode_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      BPS_Plaintext : BPS.Numerals(1..10) := (0,1,2,3,4,5,6,7,8,9);
      BPS_Ciphertext, P : BPS.Numerals(1..10);
   begin
      for i in B_Block128'Range loop
         Crypto.Random.Read(BPS_Key(i));
      end loop;
      for j in B_Block64'Range loop
         Crypto.Random.Read(BPS_IV(j));
      end loop;

      Init(BPS_Key, BPS_IV);
      Encrypt(BPS_Plaintext(1..5), BPS_Ciphertext(1..5));
      Decrypt(BPS_Ciphertext(1..5), P(1..5));
      Set_IV(BPS_IV);
      Encrypt(BPS_Plaintext(6..10), BPS_Ciphertext(6..10));
      Decrypt(BPS_Ciphertext(6..10), P(6..10));

      Assert(P = BPS_Plaintext, "BPS Mode failed.");

   end BPS_Mode_Test2;

------------------------------------------------------------------------------------
end Test.BPS_Mode;
