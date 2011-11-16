with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants;
with Big_Number_Add_Results;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

package body Test.Big_Number_Add is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------
   
   package Big is new Crypto.Types.Big_Numbers(4096);
   use Big;
   use Big.Utils;
   
   use Big_Number_Constants;
   use Big_Number_Add_Results;
   
   X_4096, X_3812, X_1025, X_1024, X_768, X_1, X_0: Big_Unsigned;
   X, Result: Big_Unsigned;
	
   -----------------------------------------------------------------------------
   ------------------------------- Constants -----------------------------------
   -----------------------------------------------------------------------------
   
   procedure Constants is
   begin
      X_4096 := To_Big_Unsigned(Cons_4096);
      X_3812 := To_Big_Unsigned(Cons_3812);
      X_1025 := To_Big_Unsigned(Cons_1025);
      X_1024 := To_Big_Unsigned(Cons_1024);
      X_768  := To_Big_Unsigned(Cons_768);
      X_1 := To_Big_Unsigned("1");
      X_0 := To_Big_Unsigned("0");
   end Constants;

   -----------------------------------------------------------------------------
   ------------------------ Register Big_Number3 Tests -------------------------
   -----------------------------------------------------------------------------
	
   procedure Register_Tests(T : in out Big_Number_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Big_Number_Add_Test1'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test2'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test3'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test4'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test5'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test6'Access,"Addition");
      Register_Routine(T, Big_Number_Add_Test7'Access,"Addition");      
   end Register_Tests;
   
   -----------------------------------------------------------------------------
   --------------------------- Name Big_Number4 Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Big_Number_Test) return Test_String is
   begin
      return new String'("Big Number Tests");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------
   
   procedure Big_Number_Add_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      Constants;
      X := X_4096 + X_4096;
      Result := To_Big_Unsigned(Result_3);
      Assert(X = Result, "Failed with Big Number 4096 Bit.");
      X := X_4096 + X_3812;
      Result := To_Big_Unsigned(Result_4);
      Assert(X = Result, "Failed with Big Number 4096 and 3812 Bit.");
      X := X_4096 + X_1025;
      Assert(X = X_1024, "Failed with Big Number 4096 and 1025 Bit.");
      X := X_4096 + X_1024;
      Result := To_Big_Unsigned(Result_5);
      Assert(X = Result, "Failed with Big Number 4096 and 1024 Bit.");
      X := X_4096 + X_768;
      Result := To_Big_Unsigned(Result_6);
      Assert(X = Result, "Failed with Big Number 4096 and 768 Bit.");
      X := X_4096 + X_1;
      Assert(X = X_0, "Failed with Big Number 4096 and 1 Bit.");
      X := X_4096 + X_0;
      Assert(X = X_4096, "Failed with Big Number 4096 and 0 Bit.");
   end Big_Number_Add_Test1;

   -----------------------------------------------------------------------------
   ------------------------------- Test 2 --------------------------------------
   -----------------------------------------------------------------------------

   procedure Big_Number_Add_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      X := X_3812 + X_4096;
      Result := To_Big_Unsigned(Result_4);
      Assert(X = Result, "Failed with Big Number 4096 and 3812 Bit.");
      
      X := X_3812 + X_3812;
      Result := To_Big_Unsigned(Result_0);
      Assert(X = Result, "Failed with Big Number 3812 Bit.");
      
      X := X_3812 + X_1025;
      Result := To_Big_Unsigned(Result_1);
      Assert(X = Result, "Failed with Big Number 3812 and 1025 Bit.");
   	   
      X := X_3812 + X_1024;
      Result := To_Big_Unsigned(Result_8);
      Assert(X = Result, "Failed with Big Number 3812 and 1024 Bit.");

      X := X_3812 + X_768;
      Result := To_Big_Unsigned(Result_2);
      Assert(X = Result, "Failed with Big Number 3812 and 768 Bit.");
      
      X := X_3812 + X_1;
      Result := To_Big_Unsigned(Result_7);
      Assert(X = Result, "Failed with Big Number 3812 and 1 Bit.");
      
      X := X_3812 + X_0;
      Assert(X = X_3812, "Failed with Big Number 3812 and 0 Bit.");
   end Big_Number_Add_Test2;
	   
   -----------------------------------------------------------------------------
   ------------------------------- Test 3 --------------------------------------
   -----------------------------------------------------------------------------

   procedure Big_Number_Add_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      X := X_1025 + X_4096;
      Assert(X = X_1024, "Failed with Big Number 4096 Bit and 1025 Bit.");
      
      X := X_1025 + X_3812;
      Result := To_Big_Unsigned(Result_1);
      Assert(X = Result, "Failed with Big Number 1025 and 3812 Bit.");
   	   
      X := X_1025 + X_1025;
      Result := To_Big_Unsigned("35953862697246318154586103815780494672359539578" &
	   "8461314546860162315465351611001926265416954644815072042240227759742786715" &
	   "3175795376288332449856948612789482487555357868497309705526044392024921882" &
	   "3890616590417001153767630136468492576294782622108165447432670102136917259" &
	   "6479894491876959432609670712659248448274432");
      Assert(X = Result, "Failed with Big Number 1025 Bit.");
   	   
      X := X_1025 + X_1024;
      Result := To_Big_Unsigned("35953862697246318154586103815780494672359539578" &
	   "8461314546860162315465351611001926265416954644815072042240227759742786715" &
	   "3175795376288332449856948612789482487555357868497309705526044392024921882" &
	   "3890616590417001153767630136468492576294782622108165447432670102136917259" &
	   "6479894491876959432609670712659248448274431");
      Assert(X = Result, "Failed with Big Number 1025 and 1024 Bit.");

      X := X_1025 + X_768;
      Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805502015650800778103320278931575370765888510054" &
	   "2699288208524426734455338075258050027865965399028159730069993506744533775" &
	   "8661108736356524056167271862521521319428530515509054944564870077703927699" &
	   "6712529772227603087951713249176278040195066");
      Assert(X = Result, "Failed with Big Number 1025 and 768 Bit.");

      X := X_1025 + X_1;
      Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789423" &
	   "0657273430081157732675805500963132708477322407536021120113879871393357658789" &
	   "7688144166224928474306394741243777678934248654852763022196012460941194530829" &
	   "5208500576883815068234246288147391311054082723716335051068458629823994724593" &
	   "8479716304835356329624224137217");
      Assert(X = Result, "Failed with Big Number 1025 and 1 Bit.");

      X := X_1025 + X_0;
      Assert(X = X_1025, "Failed with Big Number 1025 and 0 Bit.");
   end Big_Number_Add_Test3;

   ---------------------------------------------------------------------------------
   ----------------------------------- Test 4 --------------------------------------
   ---------------------------------------------------------------------------------
   
   procedure Big_Number_Add_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
      X := X_1024 + X_4096;
      Result := To_Big_Unsigned(Result_5);
      Assert(X = Result, "Failed with Big Number 4096 Bit and 1024 Bit.");
      
      X := X_1024 + X_3812;
      Result := To_Big_Unsigned(Result_8);
      Assert(X = Result, "Failed with Big Number 1024 and 3812 Bit.");
      
      X := X_1024 + X_1025;
      Result := To_Big_Unsigned("35953862697246318154586103815780494672359539578" &
	   "8461314546860162315465351611001926265416954644815072042240227759742786715" &
	   "3175795376288332449856948612789482487555357868497309705526044392024921882" &
	   "3890616590417001153767630136468492576294782622108165447432670102136917259" &
	   "6479894491876959432609670712659248448274431");
      Assert(X = Result, "Failed with Big Number 1024 and 1025 Bit.");
	   
      X := X_1024 + X_1024;
      Result := To_Big_Unsigned("35953862697246318154586103815780494672359539578" &
	   "8461314546860162315465351611001926265416954644815072042240227759742786715" &
	   "3175795376288332449856948612789482487555357868497309705526044392024921882" &
	   "3890616590417001153767630136468492576294782622108165447432670102136917259" &
	   "6479894491876959432609670712659248448274430");
      Assert(X = Result, "Failed with Big Number 1024 Bit.");
      
      X := X_1024 + X_768;
      Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805502015650800778103320278931575370765888510054" &
	   "2699288208524426734455338075258050027865965399028159730069993506744533775" &
	   "8661108736356524056167271862521521319428530515509054944564870077703927699" &
	   "6712529772227603087951713249176278040195065");
      Assert(X = Result, "Failed with Big Number 1024 and 768 Bit.");
      
      X := X_1024 + X_1;
      Assert(X = X_1025, "Failed with Big Number 1024 and 1 Bit.");
      
      X := X_1024 + X_0;
      Assert(X = X_1024, "Failed with Big Number 1024 and 0 Bit.");
   end Big_Number_Add_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Add_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   X := X_768 + X_4096;
	   Result := To_Big_Unsigned(Result_6);
   	   Assert(X = Result, "Failed with Big Number 4096 Bit and 768 Bit.");
   	   
   	   X := X_768 + X_3812;
	   Result := To_Big_Unsigned(Result_2);
   	   Assert(X = Result, "Failed with Big Number 768 and 3812 Bit.");
   	   
   	   X := X_768 + X_1025;
	   Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805502015650800778103320278931575370765888510054" &
	   "2699288208524426734455338075258050027865965399028159730069993506744533775" &
	   "8661108736356524056167271862521521319428530515509054944564870077703927699" &
	   "6712529772227603087951713249176278040195066");
   	   Assert(X = Result, "Failed with Big Number 768 and 1025 Bit.");

   	   X := X_768 + X_1024;
	   Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789" &
	   "4230657273430081157732675805502015650800778103320278931575370765888510054" &
	   "2699288208524426734455338075258050027865965399028159730069993506744533775" &
	   "8661108736356524056167271862521521319428530515509054944564870077703927699" &
	   "6712529772227603087951713249176278040195065");
   	   Assert(X = Result, "Failed with Big Number 768 and 1024 Bit.");

   	   X := X_768 + X_768;
	   Result := To_Big_Unsigned("21050361846015618254858209105137720342333932222" &
	   "7810407605210190537275377266175681765729295590097546139426214641456693431" &
	   "6008822960469585669135885745500625622784089099444416970700532709381396945" &
	   "165052578246743293755785693307632115700");
   	   Assert(X = Result, "Failed with Big Number 768 Bit.");

   	   X := X_768 + X_1;
   	   Result := To_Big_Unsigned("105251809230078091274291045525688601711669661" &
   	   "11390520380260509526863768863308784088286464779504877306971310732072834" &
   	   "67158004411480234792834567942872750312811392044549722208485350266354690" &
   	   "698472582526289123371646877892846653816057851");
   	   Assert(X = Result, "Failed with Big Number 768 and 1 Bit.");

   	   X := X_768 + X_0;
   	   Assert(X = X_768, "Failed with Big Number 768 and 0 Bit.");

   end Big_Number_Add_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Add_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
  	   X := X_1 + X_4096;
   	   Assert(X = X_0, "Failed with Big Number 1 and 4096 Bit.");
   	   
   	   X := X_1 + X_3812;
	   Result := To_Big_Unsigned(Result_7);
   	   Assert(X = Result, "Failed with Big Number 1 and 3812 Bit.");
   	   
   	   X := X_1 + X_1025;
	   Result := To_Big_Unsigned("17976931348623159077293051907890247336179769789423" &
	   "0657273430081157732675805500963132708477322407536021120113879871393357658789" &
	   "7688144166224928474306394741243777678934248654852763022196012460941194530829" &
	   "5208500576883815068234246288147391311054082723716335051068458629823994724593" &
	   "8479716304835356329624224137217");
   	   Assert(X = Result, "Failed with Big Number 1 and 1025 Bit.");
   	   
   	   X := X_1 + X_1024;
   	   Assert(X = X_1025, "Failed with Big Number 1 and 1024 Bit.");

   	   X := X_1 + X_768;
	   Result := To_Big_Unsigned("10525180923007809127429104552568860171166966111390" &
	   "5203802605095268637688633087840882864647795048773069713107320728346715800441" &
	   "1480234792834567942872750312811392044549722208485350266354690698472582526289" &
	   "123371646877892846653816057851");
   	   Assert(X = Result, "Failed with Big Number 1 and 768 Bit.");
   	   
   	   X := X_1 + X_1;
   	   Assert(X = 2, "Failed with Big Number 1 Bit.");
   	   
   	   X := X_1 + X_0;
   	   Assert(X = X_1, "Failed with Big Number 1 and 0 Bit.");

   end Big_Number_Add_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Add_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

  	   X := X_0 + X_4096;
   	   Assert(X = X_4096, "Failed with Big Number 0 and 4096 Bit.");
   	   
   	   X := X_0 + X_3812;
   	   Assert(X = X_3812, "Failed with Big Number 0 and 3812 Bit.");
   	   
   	   X := X_0 + X_1025;
   	   Assert(X = X_1025, "Failed with Big Number 0 and 1025 Bit.");
   	   
   	   X := X_0 + X_1024;
   	   Assert(X = X_1024, "Failed with Big Number 0 and 1024 Bit.");

   	   X := X_0 + X_768;
   	   Assert(X = X_768, "Failed with Big Number 0 and 768 Bit.");
   	   
   	   X := X_0 + X_1;
   	   Assert(X = X_1 and X = 1, "Failed with Big Number 0 and 1 Bit.");
   	   
   	   X := X_0 + X_0;
   	   Assert(X = X_0 and X = 0, "Failed with Big Number 0 Bit.");

   end Big_Number_Add_Test7;

------------------------------------------------------------------------------------

end Test.Big_Number_Add;
