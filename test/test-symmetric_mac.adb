with AUnit.Assertions;
with Crypto.Symmetric.Mac;
with Crypto.Types;
with Crypto.Types.Random;
with Ada.Text_IO;

package body Test.Symmetric_Mac is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register Symmetric Mac Test  --------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out Mac_Test) is
	   use Test_Cases.Registration;
        begin
           Register_Routine(T, Mac_Test1'Access,"Symmetric Mac Fill36 for Words.");
           Register_Routine(T, Mac_Test2'Access,"Symmetric Mac Fill36 for DWords.");
           Register_Routine(T, Mac_Test3'Access,"Symmetric Mac Fill36 for W_Block512.");
           Register_Routine(T, Mac_Test4'Access,"Symmetric Mac Fill36 for DW_Block512.");
           Register_Routine(T, Mac_Test5'Access,"Symmetric Mac Fill36 for DW_Block1024.");
   	   Register_Routine(T, Mac_Test6'Access,"Symmetric Mac Fill5C for Words.");
           Register_Routine(T, Mac_Test7'Access,"Symmetric Mac Fill5C for DWords.");
           Register_Routine(T, Mac_Test8'Access,"Symmetric Mac Fill5C for W_Block256.");
           Register_Routine(T, Mac_Test9'Access,"Symmetric Mac Fill5C for W_Block512.");
           Register_Routine(T, Mac_Test10'Access,"Symmetric Mac Fill5C for DW_Block512.");
           Register_Routine(T, Mac_Test11'Access,"Symmetric Mac Fill5C for DW_Block1024.");
           Register_Routine(T, Mac_Test12'Access,"Symmetric Mac Copy for Words.");
           Register_Routine(T, Mac_Test20'Access,"Symmetric Mac Copy for DWords.");
           Register_Routine(T, Mac_Test21'Access,"Symmetric Mac Copy for W_Block160.");
           Register_Routine(T, Mac_Test22'Access,"Symmetric Mac Copy for W_Block256.");
           Register_Routine(T, Mac_Test23'Access,"Symmetric Mac Copy for DW_Block512.");
           Register_Routine(T, Mac_Test24'Access,"Symmetric Mac Copy for DW_Block512.");

        end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Symmetric Mac Test -----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Mac_Test) return Test_String is
	begin
		return new String'("Symmetric Mac Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Words_1 : Words(1..4);
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill36(Words_1);
      for i in Words_1'Range loop
         if Words_1(i) /= 16#36_36_36_36# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill36 for Words failed!");

   end Mac_Test1;
------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DWords_1 : DWords(1..4);
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill36(DWords_1);
      for i in DWords_1'Range loop
         if DWords_1(i) /= 16#36_36_36_36_36_36_36_36# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill36 for DWords failed!");

   end Mac_Test2;
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      W_Block : W_Block512 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill36(W_Block);
      for i in W_Block'Range loop
         if W_Block(i) /= 16#36_36_36_36# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill36 for W_Block512 failed!");

   end Mac_Test3;
------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Block : DW_Block512 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill36(DW_Block);
      for i in DW_Block'Range loop
         if DW_Block(i) /= 16#36_36_36_36_36_36_36_36# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same ,"Symmetric Mac Test Fill36 for DW_Block512 failed!");

   end Mac_Test4;
------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Block_1024 : DW_Block1024 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill36(DW_Block_1024);
      for i in DW_Block_1024'Range loop
         if DW_Block_1024(i) /= 16#36_36_36_36_36_36_36_36# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same ,"Symmetric Mac Test Fill36 for DW_Block1024 failed!");

   end Mac_Test5;
------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Words_2 : Words(1..4);
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(Words_2);
      for i in Words_2'Range loop
         if Words_2(i) /= 16#5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for Words failed!");

   end Mac_Test6;
------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DWords_2 : DWords(1..4);
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(DWords_2);
      for i in DWords_2'Range loop
         if DWords_2(i) /= 16#5C_5C_5C_5C_5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for DWords failed!");

   end Mac_Test7;
------------------------------------------------------------------------------------
-------------------------------------- Test 8 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test8(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      W_Block_256 : W_Block256 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(W_Block_256);
      for i in W_Block_256'Range loop
         if W_Block_256(i) /= 16#5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for W_Block256 failed!");

   end Mac_Test8;
------------------------------------------------------------------------------------
-------------------------------------- Test 9 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test9(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      W_Block_512 : W_Block512 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(W_Block_512);
      for i in W_Block_512'Range loop
         if W_Block_512(i) /= 16#5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for W_Block512 failed!");

   end Mac_Test9;
------------------------------------------------------------------------------------
-------------------------------------- Test 10 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test10(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Block_512 : DW_Block512 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(DW_Block_512);
      for i in DW_Block_512'Range loop
         if DW_Block_512(i) /= 16#5C_5C_5C_5C_5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for DW_Block512 failed!");

   end Mac_Test10;
------------------------------------------------------------------------------------
-------------------------------------- Test 11 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test11(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      DW_Block_1024 : DW_Block1024 ;
      Is_Same : Boolean := True;
   begin
      Crypto.Symmetric.Mac.Fill5C(DW_Block_1024);
      for i in DW_Block_1024'Range loop
         if DW_Block_1024(i) /= 16#5C_5C_5C_5C_5C_5C_5C_5C# then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Fill5C for DW_Block1024 failed!");

   end Mac_Test11;
------------------------------------------------------------------------------------
-------------------------------------- Test 12 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test12(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_Words, Dest_Words : Words(1..4); -- Dest'Length = Source'Length
      Is_Same : Boolean := True;
   begin
      for i in Source_Words'Range loop
         Crypto.Types.Random.Read(Source_Words(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_Words, Dest_Words);

      for i in Source_Words'Range loop
         if Source_Words(i) /= Dest_Words(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same ,"Symmetric Mac Test Copy for Words failed!");

   end Mac_Test12;
   
   
------------------------------------------------------------------------------------
-------------------------------------- Test 20 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test20(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_DWords, Dest_DWords : DWords(1..4); -- Dest'Length = Source'Length
      Is_Same : Boolean := True;
   begin
      for i in Source_DWords'Range loop
         Crypto.Types.Random.Read(Source_DWords(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_DWords, Dest_DWords);

      for i in Source_DWords'Range loop
         if Source_DWords(i) /= Dest_DWords(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Copy for DWords failed!");

   end Mac_Test20;
------------------------------------------------------------------------------------
-------------------------------------- Test 21 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test21(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_Block160 : W_Block160;
      Dest_Block512 : W_Block512;
      Is_Same : Boolean := True;
   begin
      for i in Source_Block160'Range loop
         Crypto.Types.Random.Read(Source_Block160(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_Block160, Dest_Block512);

      for i in Source_Block160'Range loop
         if Source_Block160(i) /= Dest_Block512(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Copy for W_Block160 failed!");

   end Mac_Test21;
------------------------------------------------------------------------------------
-------------------------------------- Test 22 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test22(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_Block256 : W_Block256;
      Dest_Block512 : W_Block512;
      Is_Same : Boolean := True;
   begin
      for i in Source_Block256'Range loop
         Crypto.Types.Random.Read(Source_Block256(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_Block256, Dest_Block512);

      for i in Source_Block256'Range loop
         if Source_Block256(i) /= Dest_Block512(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same ,"Symmetric Mac Test Copy for W_Block256 failed!");

   end Mac_Test22;
------------------------------------------------------------------------------------
-------------------------------------- Test 23 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test23(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_Block512 : DW_Block512;
      Dest_Block512 : DW_Block512;
      Is_Same : Boolean := True;
   begin
      for i in Source_Block512'Range loop
         Crypto.Types.Random.Read(Source_Block512(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_Block512, Dest_Block512);

      for i in Source_Block512'Range loop
         if Source_Block512(i) /= Dest_Block512(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same,"Symmetric Mac Test Copy for DW_Block512 failed!");

   end Mac_Test23;
------------------------------------------------------------------------------------
-------------------------------------- Test 24 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mac_Test24(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Source_Block512 : DW_Block512;
      Dest_Block1024 : DW_Block1024;
      Is_Same : Boolean := True;
   begin
      for i in Source_Block512'Range loop
         Crypto.Types.Random.Read(Source_Block512(i));
      end loop;

      Crypto.Symmetric.Mac.Copy(Source_Block512, Dest_Block1024);

      for i in Source_Block512'Range loop
         if Source_Block512(i) /= Dest_Block1024(i) then
            Is_Same := False ;
         end if;
      end loop;

      Assert(Is_Same ,"Symmetric Mac Test Copy for DW_Block512 failed!");

   end Mac_Test24;
------------------------------------------------------------------------------------
end Test.Symmetric_Mac;
