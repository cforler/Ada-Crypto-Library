with AUnit.Assertions;
with Crypto.Symmetric.Mode.OFB;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;

pragma Elaborate_All(Crypto.Types);
pragma Elaborate_All(Crypto.Symmetric.Mode.OFB);

package body Test.OFB_Mode is
use Crypto.Types;
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   package AES128 renames Crypto.Symmetric.Blockcipher_AES128;
   package OFB is new Crypto.Symmetric.Mode.OFB(AES128);
   use OFB;
   Key: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#,
		       16#06#, 16#07#, 16#08#, 16#09#, 16#0a#, 16#0b#,
		       16#0c#, 16#0d#, 16#0e#, 16#0f#);

   Plaintext: B_Block256 := B_Block256(To_Bytes("ALL YOUR BASE ARE BELONG TO US! "));

   Zeros: B_Block128 :=	(16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#);
   Compare1: B_Block128;
   Compare2: B_Block128;


   Ciphertext: B_Block256;

   IV: B_Block128 := (16#00#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#,
		      16#77#, 16#88#, 16#99#, 16#aa#, 16#bb#, 16#cc#, 16#dd#,
		      16#ee#, 16#ff#);

   P: B_Block256;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Register OFB_Mode Test 1 --------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Mode_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, OFB_Mode_Test1'Access,"OFB_Mode_Test1.");
      Register_Routine(T, OFB_Mode_Test2'Access,"OFB_Mode_Test2.");
   end Register_Tests;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   --------------------------- Name OFB_Mode Test ------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   function Name(T : Mode_Test) return Test_String is
   begin
      return new String'("OFB_Mode Test");
   end Name;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------------- Start Tests --------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure OFB_Mode_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin
      Init(Key, IV);
      Encrypt(B_Block128(Plaintext(0 ..15)), B_Block128(Ciphertext(0 ..15)));
      Encrypt(B_Block128(Plaintext(16..31)), B_Block128(Ciphertext(16..31)));

      Set_IV(IV);
      Decrypt(B_Block128(Ciphertext(0 ..15)), B_Block128(P(0 ..15)));
      Decrypt(B_Block128(Ciphertext(16..31)), B_Block128(P(16..31)));

      for I in B_Block256'Range loop
	 Assert(P(I) = Plaintext(I), "OFB Mode failed.");
      end loop;
   end OFB_Mode_Test1;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure OFB_Mode_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin
      Init(Key, IV);
      Set_IV(IV);
      Encrypt(Zeros, Compare1);
      Set_IV(IV);
      Next_Block(Keystream => Compare2);

      for I in B_Block128'Range loop
	 Assert(Compare1(I) = Compare2(I), "OFB Mode failed.");
      end loop;
   end OFB_Mode_Test2;

   -----------------------------------------------------------------------------

end Test.OFB_Mode;
