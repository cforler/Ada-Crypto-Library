with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Numbers_Mod_Utils; 
with Ada.Text_IO; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
package body Test.Big_Number_Sub_Mod_Utils is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    use Big.Mod_Utils;
	
	use Ada.Text_IO;
	use Big_Number_Constants;	
	use Big_Numbers_Mod_Utils; 

    X_4096, X_4095, X_3812, X_2048, X_1025, X_1024, X_768, X_582, X_1, X_0: Big_Unsigned;
	A, N, X, Result:  Big_Unsigned;
	
------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Results is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
		X_3812 := To_Big_Unsigned(Cons_3812);
		X_2048 := To_Big_Unsigned(Cons_2048);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_582  := To_Big_Unsigned(Cons_582);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");
		
	end Results;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number_Mod_Utils Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Utils_Test1'Access,"Substraction with Modulo N = 2^4096-1.");
		Register_Routine(T, Big_Number_Mod_Utils_Test2'Access,"Substraction with Modulo N = 2^3812.");
		Register_Routine(T, Big_Number_Mod_Utils_Test3'Access,"Substraction with Modulo N = 2^2048.");
		Register_Routine(T, Big_Number_Mod_Utils_Test4'Access,"Substraction with Modulo N = 2^1025.");
		Register_Routine(T, Big_Number_Mod_Utils_Test5'Access,"Substraction with Modulo N = 2^1024.");
		Register_Routine(T, Big_Number_Mod_Utils_Test6'Access,"Substraction with Modulo N = 2^768.");
		Register_Routine(T, Big_Number_Mod_Utils_Test7'Access,"Substraction with Modulo N = 2^0.");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big_Number_Mod_Utils Test ------------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big_Number_Mod_Utils Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   Results;

   	   X := Sub(X_4096, X_0, X_4096);
   	   Assert(X = X_0, "Failed with 2^4096-1.");
   	   X := Sub(X_4096, X_1, X_4096);
   	   Assert(X = (X_4096 - X_1), "Failed with 2^4096-1.");
   	   
   	   X := Sub(X_4095, X_3812, X_4096);
   	   Assert(X = (X_4095 - X_3812), "Failed with 3812 and 4095 Bit.");

	   A := To_Big_Unsigned(Number_1);
	   X := Sub(A, X_3812, X_4096);
   	   Assert(X = (A - X_3812), "Failed with 3812 Bit.");
	   
   	   X := Sub(X_3812, X_2048, X_4096);
   	   Assert(X = (X_3812 - X_2048), "Failed with 2048 and 3812 Bit.");

   	   X := Sub(X_3812, X_1025, X_4096);
   	   Assert(X = (X_3812 - X_1025), "Failed with 1025 and 3812 Bit.");
   	   
   	   X := Sub(X_3812, X_1024, X_4096);
   	   Assert(X = (X_3812 - X_1024), "Failed with 1024 and 3812 Bit.");
   	   
   	   X := Sub(X_3812, X_768, X_4096);
   	   Assert(X = (X_3812 - X_768), "Failed with 768 and 3812 Bit.");
   	   
   	   X := Sub(X_3812, X_582, X_4096);
   	   Assert(X = (X_3812 - X_582), "Failed with 3812 and 768 Bit.");
   	   
   end Big_Number_Mod_Utils_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := Sub(X_4096, X_0, X_3812);
	   N := To_Big_Unsigned(Number_2);
   	   Assert(X = N, "Failed with 4096 and 3812 Bit.");
   	   
   	   X := Sub(X_4096, X_1, X_3812);
	   N := To_Big_Unsigned(Number_2) - 1;
   	   Assert(X = N, "Failed with 4096 and 3812 Bit.");
   	   
   	   X := Sub(X_4095, X_0, X_3812);
	   N := To_Big_Unsigned(Number_2)/2 + 1;
   	   Assert(X = N, "Failed with 4095 and 3812 Bit.");

   	   X := Sub(X_3812, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 3812 Bit.");

   	   X := Sub(X_3812, X_2048, X_3812);
   	   Assert(X = (X_3812 - X_2048), "Failed with 3812 and 2048 Bit.");

   	   X := Sub(X_2048, X_1025, X_3812);
   	   Assert(X = X_2048 - X_1025, "Failed with 1025 and 2048  Bit.");

   	   X := Sub(X_1025, X_1024, X_3812);
   	   Assert(X = X_1025 - X_1024, "Failed with 1025 and 1024 Bit.");

   	   X := Sub(X_1024, X_768, X_3812);
   	   Assert(X = X_1024 - X_768, "Failed with 1024 and 768 Bit.");

   	   X := Sub(X_768, X_582, X_3812);
   	   Assert(X = X_768 - X_582, "Failed with 582 and 768 Bit.");

   	   X := Sub(X_1, X_0, X_3812);
   	   Assert(X = X_1, "Failed with 1 Bit.");

   	   X := Sub(X_0, X_0, X_3812);
   	   Assert(X = X_0, "Failed with 0 Bit.");

   end Big_Number_Mod_Utils_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Sub(X_4096, X_0, X_2048);
   	   Assert(X = X_0, "Failed with 4096 Bit.");

   	   X := Sub(X_4096, X_1, X_2048);
	   N := (X_4096 - X_1) - (X_4096 - X_1/X_2048)*(X_2048) + 1;
   	   Assert(X = N, "Failed with 4096 - 1 Bit.");
	   
   	   X := Sub(X_4095, X_0, X_2048);
	   N := To_Big_Unsigned(Number_3);
   	   Assert(X = N, "Failed with 4095 Bit.");
	   
   	   X := Sub(X_4095, X_768, X_2048);
	   N := To_Big_Unsigned(Number_3) - X_768;
   	   Assert(X = N, "Failed with 4095 Bit.");
	   
   	   X := Sub(X_3812, X_0, X_2048);
	   N := To_Big_Unsigned(Number_4);
   	   Assert(X = N, "Failed with 3812 Bit.");
	   
   	   X := Sub(X_3812, X_768, X_2048);
	   N := (X_3812 - X_768) - ((X_3812 - X_768)/X_2048)*(X_2048);
   	   Assert(X = N, "Failed with 3812 and 768 Bit.");
	   
   	   X := Sub(X_3812, X_1025, X_2048);
	   N := (X_3812 - X_1025) - ((X_3812 - X_1025)/X_2048)*(X_2048);
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
   	   X := Sub(X_3812, X_1024, X_2048);
	   N := (X_3812 - X_1024) - ((X_3812 - X_1024)/X_2048)*(X_2048);
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
   end Big_Number_Mod_Utils_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Sub(X_4096, X_0, X_1025);
	   N := To_Big_Unsigned(Number_5);
   	   Assert(X = N, "Failed with 4096 Bit.");
	   
   	   X := Sub(X_4096, X_1, X_1025);
	   N := To_Big_Unsigned(Number_5) - 1;
   	   Assert(X = N, "Failed with 4096 Bit.");
	   
   	   X := Sub(X_4095, X_0, X_1025);
   	   Assert(X = X_0, "Failed with 4095 Bit.");
	   
   	   X := Sub(X_4095, X_1, X_1025);
	   N := (X_4095 - X_1) - ((X_4095 - X_1)/X_1025)*(X_1025);
   	   Assert(X = N, "Failed with 4095 and 1 Bit.");
	   
   	   X := Sub(X_3812, X_0, X_1025);
	   N := To_Big_Unsigned(Number_6);
   	   Assert(X = N, "Failed with 3812 Bit.");
	   
   	   X := Sub(X_3812, X_1, X_1025);
	   N := To_Big_Unsigned(Number_6) - 1;
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
   	   X := Sub(X_2048, X_0, X_1025);
	   N := To_Big_Unsigned(Number_7);
   	   Assert(X = N, "Failed with 2048 Bit.");
	   
   	   X := Sub(X_2048, X_1, X_1025);
	   N := (X_2048 - 1) - ((X_2048 - 1)/X_1025)*(X_1025);
   	   Assert(X = N, "Failed with 2048 and 1 Bit.");
	   
   	   X := Sub(X_1025, X_0, X_1025);
   	   Assert(X = X_0, "Failed with 1025 Bit.");
	   
   	   X := Sub(X_1025, X_1, X_1025);
   	   Assert(X = X_1025 - X_1, "Failed with 1025 and 1 Bit.");
	   
   	   X := Sub(X_0, X_1, X_1025);
   	   Assert(X = X_1025 - X_1, "Failed with -1 Bit.");
	   
   	   X := Sub(X_1, X_0, X_1025);
   	   Assert(X = X_1, "Failed with 1 Bit.");
	   
   end Big_Number_Mod_Utils_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Sub(X_4096, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 4096 Bit.");
	   
   	   X := Sub(X_4096, X_1, X_1024);
	   N := (X_4096 - 1) - ((X_4096 - 1)/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 4096 Bit.");
	   
   	   X := Sub(X_4095, X_0, X_1024);
	   N := X_4095 - (X_4095/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 4095 Bit.");
	   
   	   X := Sub(X_4095, X_1, X_1024);
	   N := (X_4095) - (X_4095/X_1024)*(X_1024) - 1; 
   	   Assert(X = N, "Failed with 4095 Bit.");

   	   X := Sub(X_4095, X_582, X_1024);
	   N := X_4095 - (X_4095/X_1024)*(X_1024) - X_582; 
   	   Assert(X = N, "Failed with 4095 and 582 Bit.");

   	   X := Sub(X_3812, X_0, X_1024);
	   N := X_3812 - (X_3812/X_1024)*(X_1024); 
   	   Assert(X = N, "Failed with 3812 Bit.");

   	   X := Sub(X_3812, X_1, X_1024);
	   N := X_3812 - (X_3812/X_1024)*(X_1024) - 1;
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");

   	   X := Sub(X_2048, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 2048 Bit.");

   	   X := Sub(X_2048, X_1, X_1024);
	   N := (X_2048 - 1) - ((X_2048 - 1)/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 2048 and 1 Bit.");

   	   X := Sub(X_1025, X_0, X_1024);
   	   Assert(X = X_1, "Failed with 1025 Bit.");

   	   X := Sub(X_1025, X_1, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 1 Bit.");

   	   X := Sub(X_1024, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 1024 Bit.");

   end Big_Number_Mod_Utils_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := Sub(X_4096, X_0, X_768);
	   N := X_4096 - (X_4096/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_4095, X_0, X_768);
	   N := X_4095 - (X_4095/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_3812, X_0, X_768);
	   N := X_3812 - (X_3812/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_2048, X_0, X_768);
	   N := X_2048 - (X_2048/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_1025, X_0, X_768);
	   N := X_1025 - (X_1025/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_1024, X_0, X_768);
	   N := X_1024 - (X_1024/X_768)*(X_768);
   	   Assert(X = N, "Failed with 1024 Bit.");

   	   X := Sub(X_1, X_0, X_768);
   	   Assert(X = X_1, "Failed with 1024 Bit.");

   	   X := Sub(X_0, X_0, X_768);
   	   Assert(X = X_0, "Failed with 1024 Bit.");

   end Big_Number_Mod_Utils_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := Sub(X_4096, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_3812, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_2048, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_1025, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_1024, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_1, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   	   X := Sub(X_0, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 Bit.");

   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_Sub_Mod_Utils;
