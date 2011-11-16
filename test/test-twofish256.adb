with AUnit.Assertions; 
with Crypto.Symmetric.Blockcipher_Twofish256;
with Crypto.Types;

package body Test.Twofish256 is
   use Crypto.Types;
   
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ---------------------------- Type - Declaration -----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   Plaintext: B_Block128 := (others => 0);
   Key: Bytes(B_Block256'Range) := (others => 0);
   Ciphertext: B_Block128 := (16#37#, 16#FE#, 16#26#, 16#FF#, 16#1C#,
                              16#F6#, 16#61#, 16#75#, 16#F5#, 16#DD#,
                              16#F4#, 16#C3#, 16#3B#, 16#97#, 16#A2#,
                              16#05#);
   PT: B_Block128 := Plaintext;
   CT: B_Block128;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Register Twofish256 Test 1 ------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   
   procedure Register_Tests(T : in out Twofish_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Twofish256_Test1'Access,"Twofish256_Test1.");
      Register_Routine(T, Twofish256_Test2'Access,"Twofish256_Test2.");
      Register_Routine(T, Twofish256_Test3'Access,"Twofish256_Test3.");
   end Register_Tests;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------- Name Twofish256 Test ------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   
   function Name(T : Twofish_Test) return Test_String is
   begin
      return new String'("Twofish256 Test");
   end Name;
   
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------------- Start Tests --------------------------------
   -----------------------------------------------------------------------------
   ---------------------------------- Test 1 -----------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish256_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish256;
   begin
      for I in 1 .. 49 loop
	 Prepare_Key(B_Block256(Key));
	 Encrypt(PT, CT);
	 Key(16 .. 31) := Key(0 .. 15);
	 Key(0 .. 15) := Bytes(PT);
	 PT := CT;
      end loop;
      
      for I in CT'Range loop
	 Assert(CT(I) = Ciphertext(I), "Twofish256 failed.");
      end loop;
   end Twofish256_Test1;
   
   -----------------------------------------------------------------------------
   --------------------------------- Test 2 ------------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish256_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish256;
   begin
      Key := (16#24#, 16#8A#, 16#7F#, 16#35#, 16#28#, 16#B1#, 16#68#, 16#AC#,
	      16#FD#, 16#D1#, 16#38#, 16#6E#, 16#3F#, 16#51#, 16#E3#, 16#0C#,
	      16#2E#, 16#21#, 16#58#, 16#BC#, 16#3E#, 16#5F#, 16#C7#, 16#14#,
	      16#C1#, 16#EE#, 16#EC#, 16#A0#, 16#EA#, 16#69#, 16#6D#, 16#48#);
      
      CT := (16#43#, 16#10#, 16#58#, 16#F4#, 16#DB#, 16#C7#, 16#F7#, 16#34#,
	     16#DA#, 16#4F#, 16#02#, 16#F0#, 16#4C#, 16#C4#, 16#F4#, 16#59#);
      
      Prepare_Key(B_Block256(Key));
      Decrypt(Ciphertext, PT);
   	   
      for I in PT'Range loop
	 Assert(PT(I) = CT(I), "Twofish256 failed.");
      end loop;
   end Twofish256_Test2;

   -----------------------------------------------------------------------------
   ------------------------------- Test 3 --------------------------------------
   -----------------------------------------------------------------------------

   procedure Twofish256_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      use Crypto.Symmetric.Blockcipher_Twofish256;
   begin
      Key := (others => 0);
      
      CT := (16#57#, 16#FF#, 16#73#, 16#9D#, 16#4D#, 16#C9#, 16#2C#, 16#1B#,
	     16#D7#, 16#FC#, 16#01#, 16#70#, 16#0C#, 16#C8#, 16#21#, 16#6F#);
       
      Prepare_Key(B_Block256(Key));
      Decrypt(CT, PT);
      
      CT := (others => 0); 
      
      for I in PT'Range loop
	 Assert(PT(I) = CT(I), "Twofish256 failed.");
      end loop;
   end Twofish256_Test3;

   -----------------------------------------------------------------------------

end Test.Twofish256;
