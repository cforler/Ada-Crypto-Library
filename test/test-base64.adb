with AUnit.Assertions; 
with Crypto.Types;

package body Test.Base64 is
   use Crypto.Types;
  
------------------------------------------------------------------------------------
----------------------------- Register Tests -----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Base64_Test) is
	   use Test_Cases.Registration;
	begin
	   Register_Routine(T,  Base64_Rest0_Test'Access,"Base64 Rest0 Test.");
	   Register_Routine(T,  Base64_Rest1_Test'Access,"Base64 Rest1 Test.");
	   Register_Routine(T,  Base64_Rest2_Test'Access,"Base64 Rest2 Test.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------ Name Base64 Test ------------------------------
------------------------------------------------------------------------------------

	function Name(T : Base64_Test) return Test_String is
	begin
		return new String'("Base64 Test");
	end Name;

---------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Base64_Rest0_Test(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 

      Input  : constant String := "any carnal pleasur";
      Output : constant Base64_String := "YW55IGNhcm5hbCBwbGVhc3Vy";
      Result : Base64_String := Encode_Base64( To_Bytes(Input) );
   begin
      Assert(Output = Result, "Base64_Rest0_Test failed"); 
   end Base64_Rest0_Test;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------


   procedure Base64_Rest1_Test(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      Input  : constant String := "any carnal pleasu";
      Output : constant Base64_String := "YW55IGNhcm5hbCBwbGVhc3U=";
      Result : Base64_String := Encode_Base64( To_Bytes(Input) );
   begin
      Assert(Output = Result, "Base64_Rest1_Test failed"); 
   end Base64_Rest1_Test;


------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------


   procedure Base64_Rest2_Test(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
      Input  : constant String := "any carnal pleas";
      Output : constant Base64_String := "YW55IGNhcm5hbCBwbGVhcw==";
      Result : Base64_String := Encode_Base64(To_Bytes(Input));
   begin
      Assert(Output = Result, "Base64_Rest2_Test failed"); 
   end Base64_Rest2_Test;


end Test.Base64;
