with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.NSS_BF;

package body Test.Elliptic_Curves_NSS_BF is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(64);
	package EC  is new Crypto.Types.Elliptic_Curves(Big);
	use Big;
	use EC;
	
	package NSS_BF is new EC.NSS_BF;
	
	A: Big.Big_Unsigned := Big_Unsigned_Zero + 8;
	B: Big.Big_Unsigned := Big_Unsigned_Zero + 9;
	Z: Big.Big_Unsigned := Big_Unsigned_Zero + 19;
	
	P1:   EC_Point := (Big_Unsigned_Zero + 2#0010#, Big_Unsigned_Zero + 2#1111#);
	P2:   EC_Point := (Big_Unsigned_Zero + 2#1100#, Big_Unsigned_Zero + 2#1100#);
	P3:   EC_Point;
	Temp: EC_Point := (Big_Unsigned_Zero + 2#0001#, Big_Unsigned_Zero + 2#0001#);
	
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register Elliptic_Curves Test 1 -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Elliptic_Curves_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, Elliptic_Curves_Test1'Access,"Elliptic_Curves_Test1.");
		Register_Routine(T, Elliptic_Curves_Test2'Access,"Elliptic_Curves_Test2.");
		Register_Routine(T, Elliptic_Curves_Test3'Access,"Elliptic_Curves_Test3.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Elliptic_Curves Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Elliptic_Curves_Test) return Test_String is
	begin
		return new String'("Elliptic_Curves Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types;
	  use NSS_BF;
   
   begin
   	   
   	   Init(A,B,Z);
	   
	   Assert(Is_Elliptic_Curve, "Elliptic Curve Failed"); 
	   Assert(On_Elliptic_Curve(P1), "Elliptic Curve Failed."); 
	   Assert(On_Elliptic_Curve(P2), "Elliptic Curve Failed.");
	   Assert(On_Elliptic_Curve(Temp), "Elliptic Curve Failed.");

   end Elliptic_Curves_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
	  use NSS_BF;
   
   begin
   	   
   	   Assert(P1 + P2 = Temp, "Elliptic Curve Addition failed.");
   
   end Elliptic_Curves_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
	  use NSS_BF;
   
   begin
   	   
   	   Temp := (Big_Unsigned_Zero + 2#1011#, Big_Unsigned_Zero + 2#0010#);
   	   Assert(Double(P1) = Temp, "Elliptic Curve Double failed.");
   
   end Elliptic_Curves_Test3;

------------------------------------------------------------------------------------

end Test.Elliptic_Curves_NSS_BF;
