with AUnit.Assertions; 
with Crypto.Asymmetric.ECDH;

pragma Elaborate_All (Crypto.Asymmetric.ECDH);

package body Test.ECDH is
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
   package ECDH is new  Crypto.Asymmetric.ECDH(544);
   use ECDH;
    
    Public_Key_A  : Public_Key_ECDH;
    Public_Key_B  : Public_Key_ECDH;
    Private_Key_A : Private_Key_ECDH;
    Private_Key_B : Private_Key_ECDH;
    Shared_Key_A  : Shared_Key_ECDH;
    Shared_Key_B  : Shared_Key_ECDH;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register ECDH Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out ECDH_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, ECDH_Test1'Access,"ECDH_Test1.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name ECDH Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : ECDH_Test) return Test_String is
	begin
		return new String'("ECDH Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure ECDH_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Gen_Public_Key(Public_Key_A, 256);
      Gen_Public_Key(Public_Key_B, 256);
      
      Gen_Single_Private_Key(Public_Key_A, Private_Key_A);
      Gen_Single_Private_Key(Public_Key_B, Private_Key_B);
   	   
      Gen_Shared_Private_Key(Public_Key_B, Private_Key_A, Shared_Key_A);
      Gen_Shared_Private_Key(Public_Key_A, Private_Key_B, Shared_Key_B);
      
      Assert(Verify(Public_Key_A, Public_Key_B, Private_Key_A, Private_Key_B, Shared_Key_A, Shared_Key_B), "Verifying ECDH Key failed.");
   end ECDH_Test1;

------------------------------------------------------------------------------------

end Test.ECDH;
