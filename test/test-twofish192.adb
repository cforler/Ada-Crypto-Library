with AUnit.Assertions; 
with Crypto.Symmetric.Blockcipher_Twofish192;
with Crypto.Types;
  
package body Test.Twofish192 is
   use Crypto.Types;
   
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------- Type - Declaration -------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   Plaintext : constant B_Block128 := (others => 0);
   Key: Bytes(B_Block192'Range) := (others => 0);
   Ciphertext: constant B_Block128 := (16#E7#, 16#54#, 16#49#, 16#21#, 16#2B#,
                              16#EE#, 16#F9#, 16#F4#, 16#A3#, 16#90#,
                              16#BD#, 16#86#, 16#0A#, 16#64#, 16#09#,
                              16#41#);
   PT: B_Block128 := Plaintext;
   CT: B_Block128;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------- Register Twofish192 Test 1 -----------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
	
   procedure Register_Tests(T : in out Twofish_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Twofish192_Test1'Access,"Twofish192_Test1.");
      Register_Routine(T, Twofish192_Test2'Access,"Twofish192_Test2.");
      Register_Routine(T, Twofish192_Test3'Access,"Twofish192_Test3.");
   end Register_Tests;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Name Twofish192 Test ------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   function Name(T : Twofish_Test) return Test_String is
   begin
      return new String'("Twofish192 Test");
   end Name;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------------- Start Tests --------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish192_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish192;
   begin
      for I in 1 .. 49 loop
	 Prepare_Key(B_Block192(Key));
	 Encrypt(PT, CT);
	 Key(16 .. 23) := Key(0 .. 7);
	 Key(0 .. 15) := Bytes(PT);
	 PT := CT;
      end loop;
      
      for I in CT'Range loop
	 Assert(CT(I) = Ciphertext(I), "Twofish192 failed.");
      end loop;
   end Twofish192_Test1;
   
   -----------------------------------------------------------------------------
   --------------------------------- Test 2 ------------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish192_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish192;
   begin
      Key := (16#FB#, 16#66#, 16#52#, 16#2C#, 16#33#, 16#2F#, 16#CC#, 16#4C#,
	      16#04#, 16#2A#, 16#BE#, 16#32#, 16#FA#, 16#9E#, 16#90#, 16#2F#,
	      16#DE#, 16#A4#, 16#F3#, 16#DA#, 16#75#, 16#EC#, 16#7A#, 16#8E#);
      
      CT := (16#F0#, 16#AB#, 16#73#, 16#30#, 16#11#, 16#25#, 16#FA#, 16#21#,
	     16#EF#, 16#70#, 16#BE#, 16#53#, 16#85#, 16#FB#, 16#76#, 16#B6#);
      
      Prepare_Key(B_Block192(Key));
      Decrypt(Ciphertext, PT);
      
      for I in PT'Range loop
	 Assert(PT(I) = CT(I), "Twofish192 failed.");
      end loop;
   end Twofish192_Test2;

   -----------------------------------------------------------------------------
   ---------------------------------- Test 3 -----------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish192_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish192;
   begin
      Key := (others => 0);
      CT := (16#EF#, 16#A7#, 16#1F#, 16#78#, 16#89#, 16#65#, 16#BD#, 16#44#,
              16#53#, 16#F8#, 16#60#, 16#17#, 16#8F#, 16#C1#, 16#91#, 16#01#);
      
      Prepare_Key(B_Block192(Key));
       Decrypt(CT, PT);
   	   
       CT := (others => 0); 
       for I in PT'Range loop
	  Assert(PT(I) = CT(I), "Twofish192 failed.");
       end loop;
   end Twofish192_Test3;
   
   -----------------------------------------------------------------------------

end Test.Twofish192;
