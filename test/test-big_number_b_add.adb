with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Numbers_Mod_Utils; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
package body Test.Big_Number_B_Add is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
	
	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    use Big.Binfield_Utils;

    use Big_Number_Constants;	
    use Big_Numbers_Mod_Utils; 

    X_4096, X_4095, X_3812, X_2048, X_1025, X_1024, X_768, X_1, X_0: Big_Unsigned;
    A, N, X :  Big_Unsigned;
	
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
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");
		
	end Results;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number_Mod_Utils Tests ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Utils_Test1'Access,"B_Add with Modulo N = 2^4096-1.");
		Register_Routine(T, Big_Number_Mod_Utils_Test2'Access,"B_Add with Modulo N = 2^3812.");
		Register_Routine(T, Big_Number_Mod_Utils_Test3'Access,"B_Add with Modulo N = 2^2048.");
		Register_Routine(T, Big_Number_Mod_Utils_Test4'Access,"B_Add with Modulo N = 2^1025.");
		Register_Routine(T, Big_Number_Mod_Utils_Test5'Access,"B_Add with Modulo N = 2^1024.");
		Register_Routine(T, Big_Number_Mod_Utils_Test6'Access,"B_Add with Modulo N = 2^768.");
		Register_Routine(T, Big_Number_Mod_Utils_Test7'Access,"B_Add with Modulo N = 2^0.");

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

   	   X := B_Add(X_4096, X_0);
   	   Assert(X = X_4096, "Failed with 4096 and 0.");
   	   X := B_Add(X_0, X_4096);
   	   Assert(X = X_4096, "Failed with 0 and 4096.");
   	   X := B_Add(X_4096, X_1);
   	   Assert(X = (X_4096 - X_1), "Failed with 4096 and 1.");
   	   X := B_Add(X_1, X_4096);
   	   Assert(X = (X_4096 - X_1), "Failed with 1 and 4096.");
   	   
   end Big_Number_Mod_Utils_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := B_Add(X_3812, X_4095);
   	   Assert(X = (X_4095 + X_3812), "Failed with 3812 and 4095 Bit.");
   	   X := B_Add(X_4095, X_3812);
   	   Assert(X = (X_4095 + X_3812), "Failed with 3812 and 4095 Bit.");

   end Big_Number_Mod_Utils_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
	   A := To_Big_Unsigned(Number_1);
   	   X := B_Add(X_3812, A);
   	   Assert(X = (X_3812 + A), "Failed with 3812 Bit.");
   	   X := B_Add(A, X_3812);
   	   Assert(X = (X_3812 + A), "Failed with 3812 Bit.");
	   
	   
   end Big_Number_Mod_Utils_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := B_Add(X_3812, X_2048);
	   N := To_Big_Unsigned("198001622277321825891239160420582306738039010065" &
	   "151758519860262575639491229410593192149328128882463459334858438193557" &
	   "880584895275217270097568455194051401199660122956008281821081139206999" &
	   "177127957560878881213974972903750293806289571041311018898700107859469" &
	   "105366161718179463965664175375367448273652235148779340725484501703109" &
	   "032641025320187904018842660793499448398141134136908428658497583293871" &
	   "616678256211963639280214296301381031145374684707564201340476906770208" &
	   "207637922175568093566952136803170318886863031607751943566439871242666" &
	   "951506671932521050868780375057360464984200141689837131099428613994297" &
	   "767468370379015243337759402554227175962465310579431668293521642568121" &
	   "489807096050121246178009785330662273248336290733546517067322278893580" &
	   "483057461745420768532011276274509469188964353702262334241660951896419" &
	   "084100496268860384402497925357834569943971077856488004064288712647816" &
	   "287561501754602585564254804490779371213801765498975399830980815862955" &
	   "194742241777099062826355385141284507479677132089576593614297590335833" &
	   "407013627792485358762153974838830945193772016012511482050121725280334" &
	   "20482756211621328383291024942407820546609642631980687029451096063");
   	   Assert(X = N, "Failed with 3812 and 2048 Bit.");
   	   X := B_Add(X_2048, X_3812);
   	   Assert(X = N, "Failed with 2048 and 3812 Bit.");

   end Big_Number_Mod_Utils_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := B_Add(X_3812, X_1025);
   	   Assert(X = (X_1025 + X_3812), "Failed with 1025 and 3812 Bit.");
   	   X := B_Add(X_1025, X_3812);
   	   Assert(X = (X_1025 + X_3812), "Failed with 3812 and 1025 Bit.");
   	   
   end Big_Number_Mod_Utils_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := B_Add(X_3812, X_1024);
	   N := To_Big_Unsigned("993207301340221318753647354250400031414135819031" &
	   "722928419683714460034974310557641378405173287780095286106477552289407" &
	   "385496692407359222117504535982179972637161011348100961996904603729745" &
	   "666788687267322685204290742002818375570174349544372908827640179338517" &
	   "37413932841629299335243899324700633390008281130336257");
	   N := X_3812 - N;
   	   Assert(X = N, "Failed with 3812 and 1024 Bit.");
   	   X := B_Add(X_1024, x_3812);
   	   Assert(X = N, "Failed with 1024 and 3812 Bit.");
   	   
   end Big_Number_Mod_Utils_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := B_Add(X_3812, X_768);
	   N := To_Big_Unsigned("104638162073303055665972474972667034765373419724" &
	   "033646288259681846461391685245472487112968704329269555822315233027797" &
	   "871154328342164435844748175661752702722931466070888801808886635307952" &
	   "2209382549677867534091455436476321419433488378");
   	   Assert(X = (X_3812 + N), "Failed with 3812 and 768 Bit.");
   	   X := B_Add(X_768, X_3812);
   	   Assert(X = (X_3812 + N), "Failed with 768 and 3812 Bit.");
   	   
   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_B_Add;
