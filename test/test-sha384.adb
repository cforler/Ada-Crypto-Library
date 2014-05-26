with AUnit.Assertions; 
with Crypto.Symmetric.Algorithm.SHA384;
with Crypto.Types;

use Crypto.Types;
package body Test.SHA384 is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   S1: constant String := "abc";
   S4: constant String := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijkl"
				& "mnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";

   File1: constant String := "hash_message1.txt";
   File3: constant String := "hash_message3.txt";
   File5: constant String := "hash_message5.txt";	

	H384: Crypto.Types.DW_Block384;
	Result_SHA384: Crypto.Types.DW_Block384;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register SHA384 Test 1 - 3 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out SHA_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA384_Test1'Access,"SHA384_Test1.");
		Register_Routine(T, SHA384_Test2'Access,"SHA384_Test2.");
		Register_Routine(T, SHA384_Test3'Access,"SHA384_Test3.");
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
	
	procedure SHA384_Test1(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions;
		use Crypto.Symmetric.Algorithm.SHA384;
	begin

		Result_SHA384 := (16#cb00753f45a35e8b#, 16#b5a03d699ac65007#, 
						  16#272c32ab0eded163#, 16#1a8b605a43ff5bed#,
						  16#8086072ba1e7cc23#, 16#58baeca134c825a7#);
        
      Hash(S1,H384);
      Assert(H384 = Result_SHA384, "Hashfunction failed with String 1.");
    
      F_Hash(File1, H384);
      Assert(H384 = Result_SHA384, "Hashfunction failed with File 1.");

        end SHA384_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA384_Test2(T: in out Test_Cases.Test_Case'Class) is
		use AUnit.Assertions;
		use Crypto.Symmetric.Algorithm.SHA384;
	begin

      Result_SHA384 := (16#09330c33f71147e8#, 16#3d192fc782cd1b47#,
        				  16#53111b173b3b05d2#, 16#2fa08086e3b0f712#,
        				  16#fcc7c71a557e2db9#, 16#66c3e9fa91746039#);

      Hash(S4, H384);
      Assert(H384 = Result_SHA384, "Hashfunction failed with String 4.");
        
      F_Hash(File5, H384);
      Assert(H384 = Result_SHA384, "Hashfunction failed with File 5.");
    
        end SHA384_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------
	
	procedure SHA384_Test3(T: in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
		use AUnit.Assertions;
		use Crypto.Symmetric.Algorithm.SHA384;
	begin
		
		Result_SHA384 := (16#9d0e1809716474cb#, 16#086e834e310a4a1c#,
                 		  16#ed149e9c00f24852#, 16#7972cec5704c2a5b#,
                 		  16#07b8b3dc38ecc4eb#, 16#ae97ddd87f3d8985#);

      F_Hash(File3, H384);
      Assert(H384 = Result_SHA384, "Hashfunction failed with File 3.");
    
        end SHA384_Test3;

------------------------------------------------------------------------------------

end Test.SHA384;
