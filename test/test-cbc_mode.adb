with AUnit.Assertions; 
with Crypto.Symmetric.Mode.CBC;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;


pragma Elaborate_All(Crypto.Types);

package body Test.CBC_Mode is
use Crypto.Types;
   -----------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------- Type - Declaration --------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

   package AES128 renames Crypto.Symmetric.Blockcipher_AES128;
   package CBC is new Crypto.Symmetric.Mode.CBC(AES128);
    use CBC;

	Key: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#,
                        16#06#, 16#07#, 16#08#, 16#09#, 16#0a#, 16#0b#,
                        16#0c#, 16#0d#, 16#0e#, 16#0f#);
   
   	Plaintext: B_Block256 := B_Block256(To_Bytes("ALL YOUR BASE ARE BELONG TO US! "));

   	Ciphertext: B_Block256;
	
   	IV: B_Block128 := (16#00#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#,
                       16#77#, 16#88#, 16#99#, 16#aa#, 16#bb#, 16#cc#, 16#dd#,
			   16#ee#, 16#ff#);
	
    P: B_Block256;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------------------- Register CBC_Mode Test 1 ---------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
	
    procedure Register_Tests(T : in out Mode_Test) is
       use Test_Cases.Registration;
    begin
       Register_Routine(T, CBC_Mode_Test1'Access,"CBC_Mode_Test1.");
    end Register_Tests;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------------------ Name CBC_Mode Test ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

	function Name(T : Mode_Test) return Test_String is
	begin
		return new String'("CBC_Mode Test");
	end Name;

--------------------------------------------------------------------------------
---------------------------------- Start Tests ---------------------------------
--------------------------------------------------------------------------------
------------------------------------ Test 1 ------------------------------------
--------------------------------------------------------------------------------

   procedure CBC_Mode_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Init(Key, IV);
      Encrypt(B_Block128(Plaintext(0 ..15)), B_Block128(Ciphertext(0 ..15)));
      Encrypt(B_Block128(Plaintext(16..31)), B_Block128(Ciphertext(16..31)));
      
      Set_IV(IV);
      Decrypt(B_Block128(Ciphertext(0 ..15)), B_Block128(P(0 ..15)));
      Decrypt(B_Block128(Ciphertext(16..31)), B_Block128(P(16..31)));
      
      for I in B_Block256'Range loop
	 Assert(P(I) = Plaintext(I), "CBC Mode failed.");
      end loop;
      
   end CBC_Mode_Test1;

------------------------------------------------------------------------------------

end Test.CBC_Mode;
