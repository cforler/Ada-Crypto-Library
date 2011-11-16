with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_SR is

------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------

	package Big is new Crypto.Types.Big_Numbers(4096);
    use Big;
    use Big.Utils;
    
	use Big_Number_Constants;
	
	X_4096, X_4095, X_1025, X_1024, X_768, X_1, X_0, P, Q, R, Z : 
	Big_Unsigned;

------------------------------------------------------------------------------------
------------------------------------ Constants -------------------------------------
------------------------------------------------------------------------------------

	procedure Constants is
	begin
	
		X_4096 := To_Big_Unsigned(Cons_4096);
		X_4095 := To_Big_Unsigned(Cons_4095);
		X_1025 := To_Big_Unsigned(Cons_1025);
		X_1024 := To_Big_Unsigned(Cons_1024);
		X_768  := To_Big_Unsigned(Cons_768);
		X_1 := To_Big_Unsigned("1");
		X_0 := To_Big_Unsigned("0");

	end Constants;

------------------------------------------------------------------------------------
---------------------------- Register Big_Number Test 1 ----------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin

		Register_Routine(T, Big_Number_Test1'Access,"Shift Right");
		Register_Routine(T, Big_Number_Test2'Access,"Shift Right");
		Register_Routine(T, Big_Number_Test3'Access,"Shift Right");
		Register_Routine(T, Big_Number_Test4'Access,"Shift Right");
		Register_Routine(T, Big_Number_Test5'Access,"Shift Right");

	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------- Name Big Number Tests ------------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number Tests");
	end Name;

------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	 
   	   Constants;
	   P := Shift_Right(X_1024,3);
	   Q := To_Big_Unsigned("22471164185778948846616314884862809170224712236778832" &
	   "15917876014471658447568762039158855966530094200264001423498392416970734872" &
	   "11018020778116059288299342655472209866781081856595377774501557617649316353" &
	   "69010625721104768835292807860184239138817603404645418813835573287279993405" &
	   "742309964538104419541203028017151");
	   Assert(P = Q, "Failed with 1024 Bit shifted right 3 times.");
	   
	   P := Shift_Right(X_1024,187);
	   Q := To_Big_Unsigned("91644492539119875854140108587759483170309565350946088" &
	   "09421263904739540481712929330990490965063881080137395042950079910423806995" &
	   "44226348054414240833330066547856411977855334787272537640367327320839841280" &
	   "263415847919229777715415602675691481187407177449471");
	   Assert(P = Q, "Failed with 1024 Bit shifted Right 187 times.");

	   P := Shift_Right(P,6); 
	   Q := To_Big_Unsigned("14319451959237480602209391966837419245360869586085326" &
	   "26472072485115553200267645207967264213291231418771467975460949986003719843" &
	   "03785366883502225130207822898102564371539896060511334006307394893881225200" &
	   "04115872623737965276803368791807679393553237147647");
	   Assert(P = Q, "Failed with 837 Bit shifted Right 6 times.");

   end Big_Number_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   Q := To_Big_Unsigned("2#1000010001#");
   	   P := Shift_Right(Q,2);
   	   Assert(P = 132, "Failed with 9 Bit shifted 2 times.");
   
   	   P := Shift_Right(P,3);
   	   Assert(P = 16, "Failed with 7 Bit shifted 3 times.");
   
   end Big_Number_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
	   
   	   P := Shift_Right(X_1025,167);
	   Q := To_Big_Unsigned("96096215408700162943630818502518487824790522797433645" &
	   "26947711220176168400152616586172685054182824167486149144556422992152553844" &
	   "05286687137505466996049907860485085046091635529899088428785810612776957410" &
	   "293491536147754283397719630991265870577566668501257551872");
   	   Assert(P = Q, "Failed with 1025 Bit shifted 167 times.");
   
   end Big_Number_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   P := Shift_Right(X_4096,3072);
   	   Assert(P = X_1024, "Failed with 4096 Bit shifted 3072 times");
   
   end Big_Number_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   P := Shift_Right(X_0,1000);
   	   Assert(P = X_0, "Failed with 0 Bit shifted 1000 times.");

   end Big_Number_Test5;

------------------------------------------------------------------------------------

end Test.Big_Number_SR;
