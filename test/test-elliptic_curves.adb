with Crypto.Types.Elliptic_Curves.Zp.Database;
with Crypto.Symmetric.Hashfunction_SHA1;
with Crypto.Types.Big_Numbers;
with AUnit.Assertions;
with Crypto.Types;
with Ada.Text_IO; 

pragma Elaborate_All (Crypto.Types.Big_Numbers);
pragma Elaborate_All (Crypto.Types.Elliptic_Curves);
pragma Elaborate_All (Crypto.Types.Elliptic_Curves.Zp);
pragma Elaborate_All (Crypto.Types.Elliptic_Curves.Zp.Database);
  
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
  -----------------------------------------------------------------------------
   ------------------------ Register Elliptic_curves Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Elliptic_curves_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Elliptic_curves_Test1'Access,"Elliptic_curves Known Answer Test 1");
      Register_Routine(T, Elliptic_curves_Test5'Access,"Elliptic_curves Known Answer Test 5");
      Register_Routine(T, Elliptic_curves_Put'Access,"Elliptic_curves Put Line Test");
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
      length: constant DB.Bit_Length := 5;

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
      length: constant DB.Bit_Length := 524;

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
   ----------------------------------- Put ----------------------------------
   -----------------------------------------------------------------------------

   procedure Elliptic_curves_Put(T : in out Test_Cases.Test_Case'Class) is
      use Crypto.Symmetric.Hashfunction_SHA1;
      use AUnit.Assertions;
      use Crypto.Types;
      use Ada.Text_IO;
      
      	A: Big.Big_Unsigned;
	B: Big.Big_Unsigned;
	Z: Big.Big_Unsigned;

	P1:   EC_Point;
	Put_File : File_Type; 
	
	Stdout : constant File_Type := Standard_Output;
	Put_File_Name : constant String := "ec_put_test.txt";
	
	Result : constant Crypto.Types.W_Block160 := 
	  (16#3469_619f#, 16#6f3e_f428#, 16#924b_be5a#, 16#2c85_1ef2#,
	   16#c5d6_1c7e#);
   begin
      A := A + 4;
      B := B + 20;
      Z := Z + 29;

      P1 := (X => (A+1),Y => (B+2));

      Init(A,B,Z);
      Create(Put_File, Out_File, Put_File_Name); 
      Set_Output(Put_File);
      
      EC.Put(Item => P1, Base => 10);
      EC.Put_Line(Item => P1, Base => 10);
      EC.Put_Line(Item => P1, Base => 16);
      EC.Put(Item => P1, Base => 16);
      
      Set_Output(Stdout);
      Close(Put_File);
      
      Assert(F_Hash(Put_File_Name) = Result, "Elliptic_curves Put Test failed");
   end Elliptic_curves_Put;

   -----------------------------------------------------------------------------

end Test.Elliptic_curves;
