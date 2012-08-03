with AUnit.Assertions; 
with Crypto.Symmetric.Blockcipher_Blowfish128;
with Crypto.Symmetric.Mode.CBC;
with Ada.Text_IO;
with Crypto.Types;

pragma Elaborate_All (Crypto.Symmetric.Mode.CBC);
package body Test.Blowfish is
   use Crypto.Types;
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Type - Declaration --------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
	
	package BIO is new Ada.Text_IO.Modular_IO (Byte);
	use BIO;
	package Blowfish128 renames Crypto.Symmetric.Blockcipher_Blowfish128;
	package CBC_Blowfish128 is new  Crypto.Symmetric.Mode.CBC(Blowfish128);
							
	use CBC_Blowfish128;
	
	Plaintext: array(1 .. 4) of B_Block64 :=
        ((16#37#, 16#36#, 16#35#, 16#34#, 16#33#, 16#32#, 16#31#, 16#20#),
         (16#4E#, 16#6F#, 16#77#, 16#20#, 16#69#, 16#73#, 16#20#, 16#74#),
         (16#68#, 16#65#, 16#20#, 16#74#, 16#69#, 16#6D#, 16#65#, 16#20#),
         (16#66#, 16#6F#, 16#72#, 16#20#, 16#00#, 16#00#, 16#00#, 16#00#));
    
    Ciphertext: array(1 .. 4) of B_Block64 :=
        ((16#6B#, 16#77#, 16#B4#, 16#D6#, 16#30#, 16#06#, 16#DE#, 16#E6#),
         (16#05#, 16#B1#, 16#56#, 16#E2#, 16#74#, 16#03#, 16#97#, 16#93#),
         (16#58#, 16#DE#, 16#B9#, 16#E7#, 16#15#, 16#46#, 16#16#, 16#D9#),
         (16#59#, 16#F1#, 16#65#, 16#2B#, 16#D5#, 16#FF#, 16#92#, 16#CC#));
    
    Key: B_Block128 := (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#,
                        16#CD#, 16#EF#, 16#F0#, 16#E1#, 16#D2#, 16#C3#,
                        16#B4#, 16#A5#, 16#96#, 16#87#);
    
    IV: B_Block64 := (16#FE#, 16#DC#, 16#BA#, 16#98#,
                      16#76#, 16#54#, 16#32#, 16#10#);
    
    Temp :  array(1..4) of B_Block64;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register Blowfish Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Blowfish128_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Blowfish_Test1'Access,"Blowfish Encryption Test.");
		Register_Routine(T, Blowfish_Test2'Access,"Blowfish Decryption Test.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name template Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Blowfish128_Test) return Test_String is
	begin
		return new String'("Blowfish Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Blowfish_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;   
   begin
   	   
   	   Init(Key, IV);
   	   Encrypt(Plaintext(1), Temp(1));
   	   Assert(Temp(1) = Ciphertext(1), "Blowfish Encryption failed.");
   	   Encrypt(Plaintext(2), Temp(2));
   	   Assert(Temp(2) = Ciphertext(2), "Blowfish Encryption failed.");
   	   Encrypt(Plaintext(3), Temp(3));
   	   Assert(Temp(3) = Ciphertext(3), "Blowfish Encryption failed.");
   	   Encrypt(Plaintext(4), Temp(4));
   	   Assert(Temp(4) = Ciphertext(4), "Blowfish Encryption failed.");

   end Blowfish_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Blowfish_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Set_IV(IV);
      Decrypt(Ciphertext(1), Temp(1));
      Assert(Temp(1) = Plaintext(1), "Blowfish Decryption failed.");
      Decrypt(Ciphertext(2), Temp(2));
      Assert(Temp(2) = Plaintext(2), "Blowfish Decryption failed.");
      Decrypt(Ciphertext(3), Temp(3));
      Assert(Temp(3) = Plaintext(3), "Blowfish Decryption failed.");
      Decrypt(Ciphertext(4), Temp(4));
      Assert(Temp(4) = Plaintext(4), "Blowfish Decryption failed.");
   end Blowfish_Test2;

------------------------------------------------------------------------------------

end Test.Blowfish;
