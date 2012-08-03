with AUnit.Assertions;
with Crypto.Types.Big_Numbers;
with Crypto.Types.Elliptic_Curves.NSS_BF;

pragma Elaborate_All (Crypto.Types.Big_Numbers);
pragma Elaborate_All (Crypto.Types.Elliptic_Curves.NSS_BF);

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
   	ECP3 : EC_Point := EC.EC_Point_Infinity;

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
      		Register_Routine(T, Elliptic_curves_Test4'Access,"Elliptic_curves_nss_bf_new Known Answer Test 1");
      		Register_Routine(T, Elliptic_curves_Test5'Access,"Elliptic_curves_nss_bf_new Known Answer Test 2");
      		Register_Routine(T, Elliptic_curves_Test6'Access,"Elliptic_curves_nss_bf_new Known Answer Test 3");
      		Register_Routine(T, Elliptic_curves_Test7'Access,"Elliptic_curves_nss_bf_new Known Answer Test 4");
      		Register_Routine(T, Elliptic_curves_Test8'Access,"Elliptic_curves_nss_bf_new Known Answer Test 5");
      		Register_Routine(T, Elliptic_curves_Test9'Access,"Elliptic_curves_nss_bf_new Known Answer Test 6");

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

   -----------------------------------------------------------------------------
   ----------------------------------- Test 4 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Test4(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

   begin

      NSS_BF.Init(A => A,
                 B => B,
                 F => Z);

      P3:=NSS_BF."-"(Left  => P2,
                    Right => P1);

      Assert(NSS_BF.On_Elliptic_Curve(P3), "Elliptic_curves_nss_bf Known Answer Test 1 failed");
   end Elliptic_curves_Test4;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 5 ----------------------------------
-----------------------------------------------------------------------------

   procedure Elliptic_curves_Test5(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

   begin

      Assert(NSS_BF."+"(Left  => P1,
                       Right => P1) = NSS_BF.Double(P1) AND
             NSS_BF."+"(Left  => NSS_BF.Negative(P1),
                           Right => P1) = ECP3 AND
             NSS_BF."+"(Left  => ECP3,
                           Right => P1) = P1 AND
             NSS_BF."+"(Left  => P1,
                           Right => ECP3) = P1
            , "Elliptic_curves_nss_bf Known Answer Test 2 failed");
   end Elliptic_curves_Test5;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 6 ----------------------------------
-----------------------------------------------------------------------------

   procedure Elliptic_curves_Test6(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

   begin

      Assert(NSS_BF."*"(Left  => Big.Big_Unsigned_Zero,
                       			       Right => P2)= EC.EC_Point_Infinity,
             "Elliptic_curves_nss_bf Known Answer Test 3 failed");
   end Elliptic_curves_Test6;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 7 ----------------------------------
   --------------------------------------------------------------------------

   procedure Elliptic_curves_Test7(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      ECP1: EC.EC_Point;
      ECP2: EC.EC_Point;
      ECP3: EC.EC_Point;

   begin

      ECP1.X 	:= Big.Utils.To_Big_Unsigned("0");
      ECP1.Y 	:= Big.Utils.To_Big_Unsigned("0");
      ECP2	:= EC.EC_Point_Infinity;

      Assert(NSS_BF.Double(X => ECP1) = ECP2,
             "Elliptic_curves_nss_bf Known Answer Test 4 failed");
   end Elliptic_curves_Test7;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 8 ----------------------------------
   --------------------------------------------------------------------------

   procedure Elliptic_curves_Test8(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      ECP1: EC.EC_Point;
      ECP2: EC.EC_Point;
      Factor: Big.Big_Unsigned;

   begin

      ECP1.X 	:= Big.Utils.To_Big_Unsigned("0");
      ECP1.Y 	:= Big.Utils.To_Big_Unsigned("10");
      Factor	:= Big.Utils.To_Big_Unsigned("2222222222222222222222222222222222222");

      ECP2 := NSS_BF."*"(Left  => Factor,
                        Right => P1);
      Assert(NSS_BF."*"(Left  => Factor,
                       Right => P1) = ECP2,
             "Elliptic_curves_nss_bf Known Answer Test 5 failed");
   end Elliptic_curves_Test8;

   -----------------------------------------------------------------------------
   ----------------------------------- Test 9 ----------------------------------
   --------------------------------------------------------------------------

   procedure Elliptic_curves_Test9(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

   begin

      Assert(NSS_BF.On_Elliptic_Curve(X => EC.EC_Point_Infinity),
             "Elliptic_curves_nss_bf Known Answer Test 5 failed");
   end Elliptic_curves_Test9;

end Test.Elliptic_Curves_NSS_BF;
