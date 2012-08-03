with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.Zp;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Elaborate_All(Crypto.Types.Elliptic_Curves.Zp);

package body Test.Elliptic_Curves_ZP is

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(64);
	package EC  is new Crypto.Types.Elliptic_Curves(Big);
	use Big;
	use EC;
	
	package ZP is new EC.ZP;
	
	A: Big.Big_Unsigned;
	B: Big.Big_Unsigned;
	Z: Big.Big_Unsigned;
	
	P1:   EC_Point;
	P2:   EC_Point;
	P3:   EC_Point;
	Temp: EC_Point := (X => (A+9),Y => (A+2));

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
		Register_Routine(T, Elliptic_Curves_Test4'Access,"Elliptic_Curves_Test4.");
		Register_Routine(T, Elliptic_Curves_Test5'Access,"Elliptic_Curves_Test5.");

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
	  use ZP;
   
   begin

   	   A := A + 4;
   	   B := B + 20;
   	   Z := Z + 29;
   	   
   	   P1 := (X => (A+1),Y => (B+2));
   	   P2 := (X => (A+12),Y => (B+7));
   	   Temp := (X => (A+9),Y => (A+2));

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
	  use ZP;
   
   begin
   	   
   	   Assert(P1 + P2 = Temp, "Elliptic Curve Addition failed.");
   
   end Elliptic_Curves_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
	  use ZP;
   
   begin
   	   
   	   Temp := (X => (A+10), Y => (A+2));
   	   Assert(Double(P1) = Temp, "Elliptic Curve Double failed.");
   
   end Elliptic_Curves_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
	  use ZP;
   
   begin
   	   
   	   Assert(Big_Unsigned_Two * P1 = Temp, "Elliptic Curve Double failed.");
   
   end Elliptic_Curves_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Elliptic_Curves_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
	  use Crypto.Types; 
	  use ZP;
   
   begin
   	   
   	   Assert(P3 + P1 = P1, "Elliptic Curve Double failed.");
   	   Assert(P1 + P3 = P1, "Elliptic Curve Double failed.");
   
   end Elliptic_Curves_Test5;

------------------------------------------------------------------------------------

end Test.Elliptic_Curves_ZP;
