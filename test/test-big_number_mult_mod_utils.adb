with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Numbers_Mod_Utils; 
with Ada.Text_IO; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
package body Test.Big_Number_Mult_Mod_Utils is

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
----------------------- Register Big_Number_Mod_Utils Tests ------------------------
------------------------------------------------------------------------------------
	
	procedure Register_Tests(T : in out Big_Number_Test) is
		use Test_Cases.Registration;
	begin
		
		Register_Routine(T, Big_Number_Mod_Utils_Test1'Access,"Multiplication with Modulo N = 2^4096-1.");
		Register_Routine(T, Big_Number_Mod_Utils_Test2'Access,"Multiplication with Modulo N = 2^3812.");
		Register_Routine(T, Big_Number_Mod_Utils_Test3'Access,"Multiplication with Modulo N = 2^2048.");
		Register_Routine(T, Big_Number_Mod_Utils_Test4'Access,"Multiplication with Modulo N = 2^1025.");
		Register_Routine(T, Big_Number_Mod_Utils_Test5'Access,"Multiplication with Modulo N = 2^1024.");
		Register_Routine(T, Big_Number_Mod_Utils_Test6'Access,"Multiplication with Modulo N = 2^768.");
		Register_Routine(T, Big_Number_Mod_Utils_Test7'Access,"Multiplication with Modulo N = 2^0.");

	end Register_Tests;

------------------------------------------------------------------------------------
-------------------------- Name Big_Number_Mod_Utils Test --------------------------
------------------------------------------------------------------------------------

	function Name(T : Big_Number_Test) return Test_String is
	begin
		return new String'("Big Number Mod Utils Tests");
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

   	   X := Mult(X_3812, X_4095, X_4096);
	   N := To_Big_Unsigned(Number_8);
   	   Assert(X = N, "Failed with 3812 and 4095 Bit.");

	   A := To_Big_Unsigned(Number_1);
	   X := Mult(X_3812, A, X_4096);
	   N := To_Big_Unsigned(Number_9);
   	   Assert(X = N, "Failed with 3812 Bit.");
	   
   	   X := Mult(X_3812, X_2048, X_4096);
	   N := To_Big_Unsigned(Number_10);
   	   Assert(X = N, "Failed with 2048 and 3812 Bit.");

   	   X := Mult(X_3812, X_1025, X_4096);
	   N := To_Big_Unsigned(Number_11);
   	   Assert(X = N, "Failed with 1025 and 3812 Bit.");
   	   
   	   X := Mult(X_3812, X_1024, X_4096);
	   N := To_Big_Unsigned(Number_12);
   	   Assert(X = N, "Failed with 1024 and 3812 Bit.");
   	   
   	   X := Mult(X_3812, X_768, X_4096);
	   N := To_Big_Unsigned(Number_13);
   	   Assert(X = N, "Failed with 768 and 3812 Bit.");
   	   
   	   X := Mult(X_3812, X_582, X_4096);
	   N := To_Big_Unsigned(Number_14);
   	   Assert(X = N, "Failed with 3812 and 582 Bit.");
   	   
   	   X := Mult(X_2048, X_2048, X_4096);
	   N := To_Big_Unsigned(Number_15);
   	   Assert(X = N, "Failed with 2048 Bit.");
   	   
   end Big_Number_Mod_Utils_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := Mult(X_4096, X_4095, X_3812);
	   N := To_Big_Unsigned(Number_16);
   	   Assert(X = N, "Failed with 4095 and 3812 Bit.");
   	   
   	   X := Mult(X_3812, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 3812 Bit.");

   	   X := Mult(X_4096, X_2048, X_3812);
	   N := To_Big_Unsigned(Number_17);
   	   Assert(X = N, "Failed with 4096 and 2048 Bit.");

   	   X := Mult(X_4095, X_2048, X_3812);
	   N := To_Big_Unsigned(Number_18);
   	   Assert(X = N, "Failed with 4095 and 2048 Bit.");

   	   X := Mult(X_4096, X_1025, X_3812);
	   N := To_Big_Unsigned(Number_19);
   	   Assert(X = N, "Failed with 4096 and 1025 Bit.");

   	   X := Mult(X_4095, X_1025, X_3812);
	   N := To_Big_Unsigned(Number_20);
   	   Assert(X = N, "Failed with 4095 and 1025 Bit.");

   	   X := Mult(X_4096, X_1024, X_3812);
	   N := To_Big_Unsigned(Number_21);
   	   Assert(X = N, "Failed with 4096 and 1024 Bit.");

   	   X := Mult(X_4095, X_1024, X_3812);
	   N := To_Big_Unsigned(Number_22);
   	   Assert(X = N, "Failed with 4095 and 1024 Bit.");

   	   X := Mult(X_4096, X_768, X_3812);
	   N := To_Big_Unsigned(Number_23);
   	   Assert(X = N, "Failed with 4096 and 768 Bit.");

   	   X := Mult(X_4095, X_768, X_3812);
	   N := To_Big_Unsigned(Number_24);
   	   Assert(X = N, "Failed with 4096 and 768 Bit.");

   	   X := Mult(X_4096, X_1, X_3812);
	   N := To_Big_Unsigned(Number_2);
   	   Assert(X = N, "Failed with 4096 Bit.");

   	   X := Mult(X_4095, X_1, X_3812);
	   N := To_Big_Unsigned(Number_2)/2 + 1;
   	   Assert(X = N, "Failed with 4095 Bit.");

   end Big_Number_Mod_Utils_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Mult(X_4096, X_4095, X_2048);
   	   Assert(X = X_0, "Failed with 4096 and 4095  Bit.");
	   
   	   X := Mult(X_4095, X_3812, X_2048);
	   N := To_Big_Unsigned(Number_25);
   	   Assert(X = N, "Failed with 4095 and 3812 Bit.");
   	   
   	   X := Mult(X_4095, X_1025, X_2048);
	   N := To_Big_Unsigned(Number_28);
   	   Assert(X = N, "Failed with 4095 and 1025 Bit.");
	   
   	   X := Mult(X_4095, X_1024, X_2048);
	   N := To_Big_Unsigned(Number_29);
   	   Assert(X = N, "Failed with 4095 and 1024 Bit.");
	   
   	   X := Mult(X_4095, X_768, X_2048);
	   N := To_Big_Unsigned(Number_27);
   	   Assert(X = N, "Failed with 4095 and 768 Bit.");
	   
   	   X := Mult(X_3812, X_1025, X_2048);
	   N := To_Big_Unsigned(Number_30);
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
   	   X := Mult(X_3812, X_1024, X_2048);
	   N := To_Big_Unsigned(Number_31);
   	   Assert(X = N, "Failed with 3812 and 1024 Bit.");
	   
   	   X := Mult(X_3812, X_768, X_2048);
	   N := To_Big_Unsigned(Number_26);
   	   Assert(X = N, "Failed with 3812 and 768 Bit.");
	   
   	   X := Mult(X_1025, X_1025, X_2048);
   	   Assert(X = X_1, "Failed with 1025 Bit.");
	   
   	   X := Mult(X_1025, X_1024, X_2048);
	   N := To_Big_Unsigned(Number_32);
   	   Assert(X = N, "Failed with 1025 and 1024 Bit.");
	   
   end Big_Number_Mod_Utils_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Mult(X_4096, X_4095, X_1025);
   	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");
	   
   	   X := Mult(X_4096, X_3812, X_1025);
	   N := To_Big_Unsigned(Number_33);
   	   Assert(X = N, "Failed with 4096 and 3812 Bit.");
   	   
   	   X := Mult(X_4096, X_2048, X_1025);
   	   Assert(X = X_1, "Failed with 4096 and 2048 Bit.");
	   
   	   X := Mult(X_4096, X_1024, X_1025);
   	   Assert(X = X_1, "Failed with 4096 and 1024 Bit.");
	   
   	   X := Mult(X_4096, X_768, X_1025);
	   N := To_Big_Unsigned(Number_34);
   	   Assert(X = N, "Failed with 4096 and 768 Bit.");
	   
   	   X := Mult(X_4096, X_1, X_1025);
	   N := To_Big_Unsigned(Number_5);
   	   Assert(X = X_1024, "Failed with 4095 and 1 Bit.");
	   
   	   X := Mult(X_3812, X_2048, X_1025);
	   N := To_Big_Unsigned(Number_33);
   	   Assert(X = N, "Failed with 3812 and 2048 Bit.");
	   
   	   X := Mult(X_3812, X_1024, X_1025);
	   N := To_Big_Unsigned(Number_33);
   	   Assert(X = N, "Failed with 3812 and 1024 Bit.");
	   
   	   X := Mult(X_3812, X_768, X_1025);
	   N := To_Big_Unsigned(Number_35);
   	   Assert(X = N, "Failed with 3812 and 768 Bit.");
	   
   	   X := Mult(X_3812, X_1, X_1025);
	   N := To_Big_Unsigned(Number_6);
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_2048, X_1024, X_1025);
   	   Assert(X = X_1, "Failed with 2048 and 1024 Bit.");

   	   X := Mult(X_2048, X_768, X_1025);
	   N := To_Big_Unsigned(Number_34);
   	   Assert(X = N, "Failed with 2048 and 768 Bit.");

   	   X := Mult(X_2048, X_1, X_1025);
   	   Assert(X = X_1024, "Failed with 2048 and 1 Bit.");

   end Big_Number_Mod_Utils_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Mult(X_4096, X_4095, X_1024);
   	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");
	   
   	   X := Mult(X_4095, X_3812, X_1024);
	   N := To_Big_Unsigned(Number_37);
   	   Assert(X = N, "Failed with 4095 and 3812 Bit.");
   	   
   	   X := Mult(X_4095, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");
	   
   	   X := Mult(X_4095, X_1025, X_1024);
	   N := To_Big_Unsigned(Number_36);
   	   Assert(X = N, "Failed with 4095 and 1025 Bit.");
	   
   	   X := Mult(X_4095, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");
	   
   	   X := Mult(X_4095, X_768, X_1024);
	   N := To_Big_Unsigned(Number_27);
   	   Assert(X = N, "Failed with 4096 and 768 Bit.");
	   
   	   X := Mult(X_4095, X_1, X_1024);
	   N := To_Big_Unsigned(Number_36);
   	   Assert(X = N, "Failed with 4095 and 1 Bit.");
	   
   	   X := Mult(X_3812, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
   	   X := Mult(X_3812, X_1025, X_1024);
	   N := X_3812 - (X_3812/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
   	   X := Mult(X_3812, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
   	   X := Mult(X_3812, X_768, X_1024);
	   N := To_Big_Unsigned(Number_38);
   	   Assert(X = N, "Failed with 3812 and 768 Bit.");
	   
   	   X := Mult(X_3812, X_1, X_1024);
	   N := X_3812 - (X_3812/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_1025, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_1025, X_768, X_1024);
   	   Assert(X = X_768, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_1025, X_1, X_1024);
   	   Assert(X = X_1, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_1, X_1, X_1024);
   	   Assert(X = X_1, "Failed with 3812 and 1 Bit.");
	   
   	   X := Mult(X_0, X_1, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 1 Bit.");
	   
   end Big_Number_Mod_Utils_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := Mult(X_4096, X_4095, X_768);
	   N := To_Big_Unsigned("584182030511070856644226251793147352089845135372" &
	   "902978266179039200528938897353115661327262245715830889142546597305888" &
	   "182821592468550620182723504755111214481260623427171337427485860424566" &
	   "44479783357427082607605731325923546357379130");
   	   Assert(X = N, "Failed with 4096 and 4095 Bit.");
	   
   	   X := Mult(X_4096, X_3812, X_768);
	   N := To_Big_Unsigned("359267346652737874193861501582555207972478274201" &
	   "358018014892263934876212891395540123648159640985957147587966445412309" &
	   "244734836013619356095257778759495649771799906928612991767013799152825" &
	   "814503901105606304566169913452301571096703880");
   	   Assert(X = N, "Failed with 4096 and 3812 Bit.");
   	   
   	   X := Mult(X_4096, X_2048, X_768);
	   N := To_Big_Unsigned("260300223720264922379172197227637029972435706246" &
	   "969362285595139879257482250816372810657405716899922322054192061850574" &
	   "070546947439670631718920853680787703415690933127629790546833242910329" &
	   "383197056604049276800372042428846392374990575");
   	   Assert(X = N, "Failed with 4096 and 2048 Bit.");
   	   
   	   X := Mult(X_4096, X_1025, X_768);
	   N := To_Big_Unsigned("740451242131584557922627034960476289077061350257" &
	   "376601159879723418700345975640567060819340265259880224039842136214212" &
	   "890183512597687599463849789655867393507025853447033623426057949106260" &
	   "28645867503193961570708477163780193707832110");
   	   Assert(X = N, "Failed with 4096 and 1025 Bit.");
   	   
   	   X := Mult(X_4096, X_1024, X_768);
	   N := To_Big_Unsigned("871521504188725174991553024718373012416547937854" &
	   "562434484900963411001428265217650683023135067835644620455716892605674" &
	   "904365731415329395705219457213993599355444685660992076516978318887436" &
	   "363559047254748140647201984944911326403575475");
   	   Assert(X = N, "Failed with 4096 and 1024 Bit.");
   	   
   	   X := Mult(X_4096, X_768, X_768);
   	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");
   	   
   	   X := Mult(X_4096, X_1, X_768);
	   N := To_Big_Unsigned("255041712325214193543620134034560633607854808310" &
	   "227263657137961617245492663224814851705276909178074099079340528299213" &
	   "542657031324674157075733464624343452806649944233433494310977742377880" &
	   "363559402774734944295153370111715521120314485");
   	   Assert(X = N, "Failed with 4096 and 1 Bit.");
   	   
   	   X := Mult(X_4095, X_3812, X_768);
	   N := To_Big_Unsigned("427987183493273961628252213505756333011516176098" &
	   "132180630644713851233253529696668829537695354084237745657402921037702" &
	   "478072777614850887939737184985163119256258711096456340197439055685661" &
	   "177389740617514605462493170594221121606694264");
   	   Assert(X = N, "Failed with 4095 and 3812 Bit.");
   	   
   	   X := Mult(X_4095, X_2048, X_768);
	   N := To_Big_Unsigned("186482484095924799802555163406921349233316540468" &
	   "591131789180563920573682236836229094869123551878002331508528309084414" &
	   "926377842624754010612139096538779530396370647151791834603389304009475" &
	   "763944859855598818117387985521839245173947540");
   	   Assert(X = N, "Failed with 4095 and 2048 Bit.");
	   
   	   X := Mult(X_4095, X_1025, X_768);
	   N := To_Big_Unsigned("961392197755556514950712433190676624909974661769" &
	   "610494078083421298195306334346040594284865468509083073019141015114456" &
	   "138257452130225208448917190773452193338330244602172555023868989861714" &
	   "844671876447441205788663220319256744509751638");
   	   Assert(X = N, "Failed with 4095 and 1025 Bit.");
	   
   	   X := Mult(X_4095, X_1024, X_768);
	   N := To_Big_Unsigned("307612295442558961807447138544953299547698952044" &
	   "970843236488964146384116837294428754108988038676180674913934147323115" &
	   "787926730727770733493766487024905310529309250210594703625704985495429" &
	   "313655883796929171955263096316975657041565470");
   	   Assert(X = N, "Failed with 4095 and 1024 Bit.");
	   
   	   X := Mult(X_4095, X_768, X_768);
   	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");
	   
   	   X := Mult(X_4095, X_1, X_768);
	   N := To_Big_Unsigned("653779902312997553143265294645723325362275709724" &
	   "639650841594457151811189497051611840175877429832902398105206867791340" &
	   "350330721402454474955150703748546882809020994391577851398164004366285" &
	   "531015992650512033833400124002281087468186168");
   	   Assert(X = N, "Failed with 4095 and 1 Bit.");
	   
   	   X := Mult(X_3812, X_2048, X_768);
	   N := To_Big_Unsigned("517205497465011641114667284727497152836048469701" &
	   "098257438092928561634021659251311111993379863798905216727911738692669" &
	   "243765463315566692305013262208760906663712311727313310327303794455114" &
	   "766197099030310483221619394924918281639008990");
   	   Assert(X = N, "Failed with 3812 and 2048 Bit.");
	   
   	   X := Mult(X_3812, X_1025, X_768);
	   N := To_Big_Unsigned("630876517673123981608256458371819597251215130986" &
	   "733501301103565263687833492131239111450161912557638078637372661672450" &
	   "198416094445335414680490845067454657858166684256897665726026163070779" &
	   "994161480450535100093124946776349581175817718");
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
   	   X := Mult(X_3812, X_1024, X_768);
	   N := To_Big_Unsigned("134169497339313932545613532942862139200661052991" &
	   "827158054706401496097539324133441576022930845375119734910533265009354" &
	   "487005375229252994896274253856624069117449168992597977098161850852283" &
	   "453885900321112193734308519040208909059133070");
   	   Assert(X = N, "Failed with 3812 and 1024 Bit.");
	   
   	   X := Mult(X_3812, X_768, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
   	   X := Mult(X_3812, X_1, X_768);
	   N := To_Big_Unsigned("496707020333810049062642925428957458050554077994" &
	   "906343246397163767590294167997797535427231067182518343726839396663095" &
	   "711410719216082419784216591210830588740717515264299688627864312218496" &
	   "540275580129422906358816427736140672116684648");
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");

   	   X := Mult(X_2048, X_1025, X_768);
	   N := To_Big_Unsigned("218348777716144187922504754814013361534566759524" &
	   "120780834553620165061618217851972614182835990151270810278901387360338" &
	   "639698240896718543689764120265478986830258583462706781357714984220464" &
	   "977366444643814473151374769861995304155802180");
   	   Assert(X = N, "Failed with 2048 and 1025 Bit.");
	   
   	   X := Mult(X_2048, X_1024, X_768);
	   N := To_Big_Unsigned("105684033244559510696566625227807693040369384833" &
	   "907879541787632203171735994995887235101994603295188469316036831042082" &
	   "857489503086881154184406780868707629453208222286752902697769619111842" &
	   "832673781536666113716970841247163206182897675");
   	   Assert(X = N, "Failed with 2048 and 1024 Bit.");
	   
   	   X := Mult(X_2048, X_768, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
   	   X := Mult(X_2048, X_1, X_768);
	   N := To_Big_Unsigned("112664744471584677225938129586205668494197374690" &
	   "212901292765987961889882222856085379080841386856082340962864556318255" &
	   "782208737809837389505357339396771357377050361175953878659945365108622" &
	   "144692663107148359434403928614832097972904505");
   	   Assert(X = N, "Failed with 2048 and 1 Bit.");
	   
   	   X := Mult(X_1025, X_1024, X_768);
	   N := To_Big_Unsigned("368961657775191928602596877214672081815347408454" &
	   "833649304689023080123076813484878553886000377339365611590713154277699" &
	   "110721007769625318223043822560954335673876546415756547925513713005199" &
	   "880939942768038156171079720925792110293349040");
   	   Assert(X = N, "Failed with 1025 and 1024 Bit.");
	   
   	   X := Mult(X_1025, X_768, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
   	   X := Mult(X_1025, X_1, X_768);
	   N := To_Big_Unsigned("796221178997173661366251707628419603795546577374" &
	   "431290014127917568143691740249615653841318960004447426503224609324023" &
	   "829492141520446864116881459708567334514565859309919539219781918458112" &
	   "962225302865399326634971085581886641495613316");
   	   Assert(X = N, "Failed with 1025 and 1 Bit.");
	   
   	   X := Mult(X_1024, X_768, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 768 Bit.");
	   
   	   X := Mult(X_1024, X_1, X_768);
	   N := To_Big_Unsigned("796221178997173661366251707628419603795546577374" &
	   "431290014127917568143691740249615653841318960004447426503224609324023" &
	   "829492141520446864116881459708567334514565859309919539219781918458112" &
	   "962225302865399326634971085581886641495613315");
   	   Assert(X = N, "Failed with 1024 and 1 Bit.");
	   
   	   X := Mult(X_768, X_1, X_768);
   	   Assert(X = X_0, "Failed with 768 and 1 Bit.");
	   
   	   X := Mult(X_768, X_0, X_768);
   	   Assert(X = X_0, "Failed with 768 and 1 Bit.");
	   
   end Big_Number_Mod_Utils_Test6;

------------------------------------------------------------------------------------
-------------------------------------- Test 7 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   X := Mult(X_4096, X_4095, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Mult(X_4096, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Mult(X_4096, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Mult(X_4096, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Mult(X_4096, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Mult(X_4096, X_768, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Mult(X_4096, X_1, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 1 Bit.");

   	   X := Mult(X_4096, X_0, X_1);
   	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Mult(X_4095, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Mult(X_4095, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Mult(X_4095, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 1025 Bit.");

   	   X := Mult(X_4095, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Mult(X_4095, X_768, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Mult(X_4095, X_1, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 1 Bit.");

   	   X := Mult(X_4095, X_0, X_1);
   	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

   	   X := Mult(X_3812, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");

   	   X := Mult(X_3812, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 1025 Bit.");

   	   X := Mult(X_3812, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");

   	   X := Mult(X_3812, X_768, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");

   	   X := Mult(X_3812, X_1, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 1 Bit.");

   	   X := Mult(X_3812, X_0, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");

   	   X := Mult(X_2048, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");

   	   X := Mult(X_2048, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");

   	   X := Mult(X_2048, X_768, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");

   	   X := Mult(X_2048, X_1, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1 Bit.");

   	   X := Mult(X_2048, X_0, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");

   	   X := Mult(X_1025, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");

   	   X := Mult(X_1025, X_768, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");

   	   X := Mult(X_1025, X_1, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 1 Bit.");

   	   X := Mult(X_1025, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");

   	   X := Mult(X_1024, X_768, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 768 Bit.");

   	   X := Mult(X_1024, X_1, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 1 Bit.");

   	   X := Mult(X_1024, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 0 Bit.");

   	   X := Mult(X_768, X_1, X_1);
   	   Assert(X = X_0, "Failed with 768 and 1 Bit.");

   	   X := Mult(X_768, X_0, X_1);
   	   Assert(X = X_0, "Failed with 768 and 0 Bit.");

   	   X := Mult(X_1, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1 and 0 Bit.");

   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_Mult_Mod_Utils;
