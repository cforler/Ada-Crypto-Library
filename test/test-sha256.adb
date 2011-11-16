with AUnit.Assertions; 
with Crypto.Hashfunction_SHA256;
with Crypto.Types;

package body Test.SHA256 is 

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
   
	S1: String := "abc";
	S2: String := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
   	  
   	File1: String :="hash_message1.txt";
   	File2: String :="hash_message2.txt";
   	File3: String :="hash_message3.txt";

	Result_SHA256: Crypto.Types.W_Block256;
	
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register SHA256 Test 1 - 3 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out SHA_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA256_Test1'Access,"SHA256_Test1.");
		Register_Routine(T, SHA256_Test2'Access,"SHA256_Test2.");
		Register_Routine(T, SHA256_Test3'Access,"SHA256_Test3.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Hashfunction Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : SHA_Test) return Test_String is
	begin
		return new String'("Hashfunction Test");
	end Name;
   
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------
   
	procedure SHA256_Test1(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions; 
		use Crypto.Hashfunction_SHA256;
		use Crypto.Types;	
	begin
		
		Result_SHA256 := (16#ba7816bf#, 16#8f01cfea#, 16#414140de#, 16#5dae2223#,
						  16#b00361a3#, 16#96177a9c#, 16#b410ff61#, 16#f20015ad#);
		
		Assert(Hash(S1) = Result_SHA256 or (F_Hash(File1) = Result_SHA256), "Hashfunction SHA256 failed with String 1 and File 1.");
		
	end SHA256_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA256_Test2(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions; 
		use Crypto.Hashfunction_SHA256;
		use Crypto.Types;	
	begin
		
		Result_SHA256 := (16#248d6a61#, 16#d20638b8#, 16#e5c02693#, 16#0c3e6039#,
						  16#a33ce459#, 16#64ff2167#, 16#f6ecedd4#, 16#19db06c1#);
		
	    Assert(Hash(S2) = Result_SHA256 or (F_Hash(File2) = Result_SHA256), "Hashfunction failed with String 2 and File 2.");
	
	end SHA256_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA256_Test3(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions; 
		use Crypto.Hashfunction_SHA256;
		use Crypto.Types;	
	begin
		
		Result_SHA256 := (16#cdc76e5c#, 16#9914fb92#, 16#81a1c7e2#, 16#84d73e67#,
						  16#f1809a48#, 16#a497200e#, 16#046d39cc#, 16#c7112cd0#);

	    Assert((F_Hash(File3) = Result_SHA256), "Hashfunction failed with File 3.");
	
	end SHA256_Test3;

------------------------------------------------------------------------------------

end Test.SHA256;
