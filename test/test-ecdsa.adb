with AUnit.Assertions; 
with Crypto.Asymmetric.ECDSA;
with Crypto.Types;

package body Test.ECDSA is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
   package ECDSA is new  Crypto.Asymmetric.ECDSA(544);
   use ECDSA;
   use Crypto.Types;
   
    Public_Key  : Public_Key_ECDSA;
    Private_Key : Private_Key_ECDSA;
    Signature   : Signature_ECDSA;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register ECDSA Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out ECDSA_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, ECDSA_Test1'Access,"ECDSA_Test1.");
		Register_Routine(T, ECDSA_Test2'Access,"ECDSA_Test2.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name ECDSA Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : ECDSA_Test) return Test_String is
	begin
		return new String'("ECDSA Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure ECDSA_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
   
   begin
   	   
   	   Gen_Public_Key(Public_Key, 192);
   	   Gen_Private_Key(Public_Key, Private_Key);
   	   Sign_File("hash_message2.txt", Public_Key, Private_Key, Signature);
   	   
   	   Assert(Verify_File("hash_message2.txt", Public_Key, Signature), "Verifying File with ECDSA failed.");
   	   Assert(Verify_Key_Pair(Private_Key, Public_Key), "Verifying Key Pair with ECDSA failed.");

   end ECDSA_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure ECDSA_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
   
   begin
   	   
   	   Gen_Public_Key(Public_Key, 192);
   	   Gen_Private_Key(Public_Key, Private_Key);

   	   Sign_File("hash_message5.txt", Public_Key, Private_Key, Signature);
   	   
   	   Assert(Verify_File("hash_message5.txt", Public_Key, Signature), "Verifying File with ECDSA failed.");
   	   Assert(Verify_Key_Pair(Private_Key, Public_Key), "Verifying Key Pair with ECDSA failed.");

   end ECDSA_Test2;

------------------------------------------------------------------------------------

end Test.ECDSA;
