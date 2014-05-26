with AUnit.Assertions; 
with Crypto.Symmetric.Hashfunction_Whirlpool;
with Crypto.Types;

package body Test.Whirlpool is 

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
---------------------------------- Type - Declaration ----------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
   
   S1: constant String := "abc";
   S6 : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
   	  
   File1: constant String :="hash_message1.txt";
   File3: constant String :="hash_message3.txt";
   File6: constant String :="hash_message6.txt";

	Result: Crypto.Types.DW_Block512;
	  
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
------------------------------- Register Whirlpool1 Test 1 - 4 -------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
   
	procedure Register_Tests(T : in out SHA_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, Whirlpool_Test1'Access,"Whirlpool_Test1.");
		Register_Routine(T, Whirlpool_Test2'Access,"Whirlpool_Test2.");
		Register_Routine(T, Whirlpool_Test3'Access,"Whirlpool_Test3.");
	end Register_Tests;

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-------------------------------- Name Hashfunction Test --------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

	function Name(T : SHA_Test) return Test_String is
	begin
		return new String'("Hashfunction Test");
	end Name;
   
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-------------------------------------- Start Tests -------------------------------------
----------------------------------------------------------------------------------------
---------------------------------------- Test 1 ----------------------------------------
----------------------------------------------------------------------------------------
   
   procedure Whirlpool_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Hashfunction_Whirlpool;
	  use Crypto.Types; 
   begin
   	   
   	   Result:= (
                 16#4E2448A4C6F486BB#, 16#16B6562C73B4020B#,
                 16#F3043E3A731BCE72#, 16#1AE1B303D97E6D4C#,
                 16#7181EEBDB6C57E27#, 16#7D0E34957114CBD6#,
                 16#C797FC9D95D8B582#, 16#D225292076D4EEF5#
				);
	   Assert(Hash(S1) = Result or (F_Hash(File1) = Result), "Hashfunction Whirlpool failed with String 1 and File 1.");

   end Whirlpool_Test1;

----------------------------------------------------------------------------------------
---------------------------------------- Test 2 ----------------------------------------
----------------------------------------------------------------------------------------

   procedure Whirlpool_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Hashfunction_Whirlpool;
	  use Crypto.Types; 
   begin
    	
    	Result := (
    			   16#0C99005BEB57EFF5#, 16#0A7CF005560DDF5D#,
    			   16#29057FD86B20BFD6#, 16#2DECA0F1CCEA4AF5#,
    			   16#1FC15490EDDC47AF#, 16#32BB2B66C34FF9AD#,
    			   16#8C6008AD677F7712#, 16#6953B226E4ED8B01#
    			  );
      
      Assert(F_Hash(File3) = Result, "Hashfunction Whirlpool1 failed with String 2 and File 2.");
	
   end Whirlpool_Test2;

----------------------------------------------------------------------------------------
---------------------------------------- Test 3 ----------------------------------------
----------------------------------------------------------------------------------------
   
	procedure Whirlpool_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Hashfunction_Whirlpool;
	  use Crypto.Types; 
        begin
    	
      Result := (
                   16#DC37E008CF9EE69B#, 16#F11F00ED9ABA2690#,
                   16#1DD7C28CDEC066CC#, 16#6AF42E40F82F3A1E#,
                   16#08EBA26629129D8F#, 16#B7CB57211B9281A6#,
                   16#5517CC879D7B9621#, 16#42C65F5A7AF01467#
                  );

      Assert(Hash(S6) = Result or (F_Hash(File6) = Result), "Hashfunction Whirlpool1 failed with String 3.");

	end Whirlpool_Test3;

end Test.Whirlpool; 
