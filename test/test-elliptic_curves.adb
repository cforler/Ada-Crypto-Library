with AUnit.Assertions;
with Crypto.Types.Elliptic_Curves.Zp.Database;
with Crypto.Types;
with Crypto.Random;
with Text_IO;
with Crypto.Types.Big_Numbers;
with Crypto.Types;


package body Test.Elliptic_curves is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   package Big is new Crypto.Types.Big_Numbers(64);
   package EC  is new Crypto.Types.Elliptic_Curves(Big);
   use Big;
   use EC;
   package ZP is new EC.ZP;
   use ZP;
   package DB is new ZP.Database;
   --package UT is new Crypto.Types.Big_Numbers.Utils(64);
   -----------------------------------------------------------------------------
   ------------------------ Register Elliptic_curves Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Elliptic_curves_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Elliptic_curves_Test1'Access,"Elliptic_curves Known Answer Test 1");
      Register_Routine(T, Elliptic_curves_Test5'Access,"Elliptic_curves Known Answer Test 5");
      Register_Routine(T, Elliptic_curves_Put'Access,"Elliptic_curves Put Line Test");
      Register_Routine(T, Elliptic_curves_Put_Line'Access,"Elliptic_curves Put Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Elliptic_curves Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Elliptic_curves_Test) return Test_String is
   begin
      return new String'("Elliptic_curves Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Test1(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      ECZ: ZP.Elliptic_Curve_Zp;
      ECP: EC.EC_Point;
      order: Big_Unsigned;
      length: DB.Bit_Length := 5;

   begin
      DB.Get_Elliptic_Curve(		     	     ECZ    => ECZ ,
                                                     ECP    => ECP ,
                                                     order  => order,
                                                     length => length);

      Assert(ECP.X = 6 AND ECP.Y = 8 AND order = 6, "Elliptic_curves Known Answer Test 1 failed");
   end Elliptic_curves_Test1;


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 5 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Test5(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      ECZ: ZP.Elliptic_Curve_Zp;
      ECP: EC.EC_Point;
      order: Big_Unsigned;
      length: DB.Bit_Length := 524;

      procedure Test_Raising_Exception is
      begin

         DB.Get_Elliptic_Curve(		     	     ECZ    => ECZ ,
                                                     ECP    => ECP ,
                                                     order  => order,
                                                     length => length);

      end Test_Raising_Exception;

	begin

      Assert_Exception(Proc    => Test_Raising_Exception'Unrestricted_Access,
                       Message => "Exception test failed");


      Assert(true, "Elliptic_curves Exception Test failed");
   end Elliptic_curves_Test5;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Put_Line ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Put_Line(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      	A: Big.Big_Unsigned;
	B: Big.Big_Unsigned;
	Z: Big.Big_Unsigned;

	P1:   EC_Point;

   begin
      A := A + 4;
      B := B + 20;
      Z := Z + 29;

      P1 := (X => (A+1),Y => (B+2));

      Init(A,B,Z);

      EC.Put_Line(Item => P1,
                  Base => 10);


      Assert(true, "Elliptic_curves Put_Line Test failed");
   end Elliptic_curves_Put_Line;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Put ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Put(T : in out Test_Cases.Test_Case'Class) is

      use AUnit.Assertions;
      use Crypto.Types;

      	A: Big.Big_Unsigned;
	B: Big.Big_Unsigned;
	Z: Big.Big_Unsigned;

	P1:   EC_Point;

   begin
      A := A + 4;
      B := B + 20;
      Z := Z + 29;

      P1 := (X => (A+1),Y => (B+2));

      Init(A,B,Z);

      EC.Put(Item => P1,
             Base => 10);


      Assert(true, "Elliptic_curves Put_Line Test failed");
   end Elliptic_curves_Put;

   -----------------------------------------------------------------------------

end Test.Elliptic_curves;
