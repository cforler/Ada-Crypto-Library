with AUnit.Assertions; 
with Crypto.Hashfunction_SHA1;
with Crypto.Types;

package body Test.SHA1 is 
   use Crypto.Types;
   
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
---------------------------------- Type - Declaration ----------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
   
	S1: String := "abc"; --24 Bits
	S2: String := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"; --448 Bits
	S3: String := "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" --432 Bits
				& "aaaaaaaaaa";-- + 80 Bits = 512 Bits 
	S4: String := "";
	File1: String :="hash_message1.txt";
   	File2: String :="hash_message2.txt";
   	File3: String :="hash_message3.txt";
   	File4: String :="hash_message4.txt";
	File5: String :="hash_message7.txt";

	Result: Crypto.Types.W_Block160;
	  
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
------------------------------- Register SHA1 Test 1 - 4 -------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
   
	procedure Register_Tests(T : in out SHA_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA_Test1'Access,"SHA_Test1.");
		Register_Routine(T, SHA_Test2'Access,"SHA_Test2.");
		Register_Routine(T, SHA_Test3'Access,"SHA_Test3.");
		Register_Routine(T, SHA_Test4'Access,"SHA_Test4.");
		Register_Routine(T, SHA_Test5'Access,"SHA_Test5.");
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
   
   procedure SHA_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Hashfunction_SHA1;
   begin

	  Result := (16#a9993e36#, 16#4706816a#, 16#ba3e2571#, 16#7850c26c#, 16#9cd0d89d#);
  	  
  	  Assert(Hash(S1) = Result or (F_Hash(File1) = Result), "Hashfunction SHA1 failed with String 1 and File 1.");

   end SHA_Test1;

----------------------------------------------------------------------------------------
---------------------------------------- Test 2 ----------------------------------------
----------------------------------------------------------------------------------------

   procedure SHA_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Hashfunction_SHA1;
    begin

	  Result := (16#84983e44#, 16#1c3bd26e#, 16#baae4aa1#, 16#f95129e5#, 16#e54670f1#);
      
      Assert(Hash(S2) = Result or (F_Hash(File2) = Result), "Hashfunction SHA1 failed with String 2 and File 2.");
	
	 end SHA_Test2;

----------------------------------------------------------------------------------------
---------------------------------------- Test 3 ----------------------------------------
----------------------------------------------------------------------------------------
   
	procedure SHA_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Hashfunction_SHA1;
	  use Crypto.Types; 
    begin
	  Result := (16#34aa973c#, 16#d4c4daa4#, 16#f61eeb2b#, 16#dbad2731#, 16#6534016f#);

      Assert((F_Hash(File3) = Result), "Hashfunction SHA1 failed with String 3.");

	end SHA_Test3;

----------------------------------------------------------------------------------------
---------------------------------------- Test 4 ----------------------------------------
----------------------------------------------------------------------------------------
   
   	procedure SHA_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Hashfunction_SHA1;
	  use Crypto.Types; 
   begin
	  Result := (16#0098ba82#, 16#4b5c1642#, 16#7bd7a112#, 16#2a5a442a#, 16#25ec644d#);
      
      Assert(Hash(S3) = Result or (F_Hash(File4) = Result), "Hashfunction SHA1 failed with String 3 and File 4.");
	
   end SHA_Test4;

----------------------------------------------------------------------------------------
---------------------------------------- Test 5 ----------------------------------------
----------------------------------------------------------------------------------------

   	procedure SHA_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Hashfunction_SHA1;
	  use Crypto.Types; 
   begin
	  Result := (16#da39a3ee#, 16#5e6b4b0d#, 16#3255bfef#, 16#95601890#, 16#afd80709#);
      
      Assert(Hash(S4) = Result or (F_Hash(File5) = Result), "Hashfunction SHA1 failed with String 4 and File 5.");
	
   end SHA_Test5;

----------------------------------------------------------------------------------------

end Test.SHA1;
