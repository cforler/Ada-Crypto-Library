with AUnit.Assertions; 
with Crypto.Symmetric.MAC.RMAC;
with Crypto.Symmetric.Oneway_Blockcipher_AES128;
with Crypto.Types;
with Crypto.Random;
  
pragma Elaborate_All(Crypto.Symmetric.Mac.Rmac);
pragma Elaborate_All(Crypto.Types);
  
package body Test.RMAC is
use Crypto.Types;
use Crypto.Random;
-----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   package AES128 renames  Crypto.Symmetric.Oneway_Blockcipher_AES128;
   package RMAC is new Crypto.Symmetric.MAC.RMAC(AES128);
   
   Key1: constant B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#,
			16#06#, 16#07#, 16#08#, 16#09#, 16#0a#, 16#0b#,
			16#0c#, 16#0d#, 16#0e#, 16#0f#);
   
   Key2: constant B_Block128 := (16#00#, 16#11#, 16#22#, 16#33#, 16#44#,
				 16#55#, 16#66#, 16#77#, 16#88#, 16#99#, 
				 16#aa#, 16#bb#, 16#cc#, 16#dd#, 16#ee#, 
				 16#ff#);
   R:   B_Block128;
    Tag: B_Block128;
    
    M: B_Block256 :=  B_Block256(To_Bytes("ALL YOUR BASE ARE BELONG TO US! "));
    Message: RMAC.Blocks(0..1) := (0 => B_Block128(M(0..15)), 
				   1 => B_Block128(M(16..31)));

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ---------------------------- Register RMAC Test 1 --------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
	
    procedure Register_Tests(T : in out RMAC_Test) is
       use Test_Cases.Registration;
    begin
       Register_Routine(T, RMAC_Test1'Access,"RMAC_Test1.");
    end Register_Tests;

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ---------------------------- Name RMAC Test --------------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------

    function Name(T : RMAC_Test) return Test_String is
    begin
       return new String'("RMAC Test");
    end Name;

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ------------------------------ Start Tests ---------------------------------
    ----------------------------------------------------------------------------
    --------------------------------- Test 1 -----------------------------------
    ----------------------------------------------------------------------------

    procedure RMAC_Test1(T : in out Test_Cases.Test_Case'Class) is
       use AUnit.Assertions; 
    begin
       RMAC.Sign(Message, Key1, Key2, R, Tag);
       Assert(RMAC.Verify(Message, Key1, Key2, R, Tag), 
	      "RMAC verifying message failed");
    end RMAC_Test1;

------------------------------------------------------------------------------------

end Test.RMAC;
