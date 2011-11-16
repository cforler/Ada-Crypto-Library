with AUnit.Assertions; 
with Crypto.Symmetric.Oneway_Blockcipher_AES128;
with Crypto.Symmetric.Mode.Oneway_OFB;
with Crypto.Types;

package body Test.OFB_Oneway_Mode is
use Crypto.Types;
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------


   package AES128 renames Crypto.Symmetric.Oneway_Blockcipher_AES128;
   package Oneway_OFB is new Crypto.Symmetric.Mode.Oneway_OFB(AES128);
   use Oneway_OFB;

   Key: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#,
		       16#06#, 16#07#, 16#08#, 16#09#, 16#0a#, 16#0b#,
                        16#0c#, 16#0d#, 16#0e#, 16#0f#);
   
   Plaintext: B_Block256 := B_Block256(To_Bytes("ALL YOUR BASE ARE BELONG TO US! "));

   Ciphertext: B_Block256;
   
   IV: B_Block128 := (16#00#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#,
		      16#77#, 16#88#, 16#99#, 16#aa#, 16#bb#, 16#cc#, 16#dd#,
		      16#ee#, 16#ff#);
    
   P: B_Block256;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------- Register OFB_Oneway_Mode Test 1 ------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
	
   procedure Register_Tests(T : in out Mode_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T,OFB_Oneway_Mode_Test1'Access,"OFB_Oneway_Mode_Test1.");
   end Register_Tests;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Name OFB_Oneway_Mode Test -------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   
   function Name(T : Mode_Test) return Test_String is
   begin
      return new String'("OFB_Oneway_Mode Test");
   end Name;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------------- Start Tests ---------------------------------
   -----------------------------------------------------------------------------
   ---------------------------------- Test 1 -----------------------------------
   -----------------------------------------------------------------------------

   procedure OFB_Oneway_Mode_Test1(T : in out Test_Cases.Test_Case'Class) is
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
   end OFB_Oneway_Mode_Test1;

   -----------------------------------------------------------------------------

end Test.OFB_Oneway_Mode;
