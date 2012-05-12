with AUnit.Assertions; 
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Types;

package body Test.SHA512 is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	S1: String := "abc";
	S4: String := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijkl"
				& "mnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";

   	File1: String := "hash_message1.txt";
   	File3: String := "hash_message3.txt";
	File5: String := "hash_message5.txt";	

	Result_SHA512: Crypto.Types.DW_Block512;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register SHA512 Test 1 - 3 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out SHA_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA512_Test1'Access,"SHA512_Test1.");
		Register_Routine(T, SHA512_Test2'Access,"SHA512_Test2.");
		Register_Routine(T, SHA512_Test3'Access,"SHA512_Test3.");
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
	
	procedure SHA512_Test1(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions;
		use Crypto.Symmetric.Hashfunction_SHA512;
		use Crypto.Types;
	
	begin
		
		Result_SHA512 := (16#ddaf35a193617aba#, 16#cc417349ae204131#,
						  16#12e6fa4e89a97ea2#, 16#0a9eeee64b55d39a#,
						  16#2192992a274fc1a8#, 16#36ba3c23a3feebbd#,
						  16#454d4423643ce80e#, 16#2a9ac94fa54ca49f#);
		
		Assert(Hash(S1) = Result_SHA512 or (F_Hash(File1) = Result_SHA512), "Hashfunction SHA512 failed with String 1 and File 1.");

	end SHA512_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA512_Test2(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions;
		use Crypto.Symmetric.Hashfunction_SHA512;
		use Crypto.Types;
	
	begin
		
      Result_SHA512 := (16#8e959b75dae313da#, 16#8cf4f72814fc143f#,
      					16#8f7779c6eb9f7fa1#, 16#7299aeadb6889018#,
      					16#501d289e4900f7e4#, 16#331b99dec4b5433a#,
      					16#c7d329eeb6dd2654#, 16#5e96e55b874be909#);
		
	  Assert(Hash(S4) = Result_SHA512 or (F_Hash(File5) = Result_SHA512), "Hashfunction SHA512 failed with String 4 and File 5.");

	end SHA512_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA512_Test3(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions;
		use Crypto.Symmetric.Hashfunction_SHA512;
		use Crypto.Types;
	
	begin
		
      Result_SHA512  := (16#e718483d0ce76964#, 16#4e2e42c7bc15b463#,
      					 16#8e1f98b13b204428#, 16#5632a803afa973eb#,
      					 16#de0ff244877ea60a#, 16#4cb0432ce577c31b#,
      					 16#eb009c5c2c49aa2e#, 16#4eadb217ad8cc09b#);

	  Assert(F_Hash(File3) = Result_SHA512, "Hashfunction SHA512 failed with File 3.");

	end SHA512_Test3;

------------------------------------------------------------------------------------

end Test.SHA512;
