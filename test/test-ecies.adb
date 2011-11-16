with AUnit.Assertions; 
with Crypto.Symmetric.Algorithm.ECIES;
with Ada.Text_IO; use Ada.Text_IO;
with Crypto.Types;

package body Test.ECIES is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	package ECIES is new  Crypto.Symmetric.Algorithm.ECIES(544);
	use ECIES;

    Public_Key_A  : ECDH.Public_Key_ECDH;
    Public_Key_B  : ECDH.Public_Key_ECDH;
    Private_Key_A : ECDH.Private_Key_ECDH;
	Private_Key_B : ECDH.Private_Key_ECDH;
 
	Cipher 		: Cipher_ECIES;
	Input		: String := "Hi there";
	Temp		: Unbounded_String;
	Output 		: Unbounded_String;
 
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register ECIES Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out ECIES_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, ECIES_Test1'Access,"ECIES_Test1.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name ECIES Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : ECIES_Test) return Test_String is
	begin
		return new String'("ECIES Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure ECIES_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
   
   begin
   	   
   	   ECDH.Gen_Public_Key(Public_Key_A, 521);
   	   ECDH.Gen_Public_Key(Public_Key_B, 521);
   	   ECDH.Gen_Single_Private_Key(Public_Key_A, Private_Key_A);
   	   ECDH.Gen_Single_Private_Key(Public_Key_B, Private_Key_B);
   	   Encrypt(Public_Key_A, Public_Key_B, Private_Key_A, Input, Cipher);
   	   Decrypt(Public_Key_B, Private_Key_A, Cipher, Output);

   	   Append(Temp, Input);
      
       for I in 1 .. Input'Length loop
       	   Assert(to_string(Temp)(I) = to_string(Output)(I), "ECIES failed.");
	   end loop;
   
   end ECIES_Test1;

------------------------------------------------------------------------------------

end Test.ECIES;
