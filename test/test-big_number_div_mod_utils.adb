with AUnit.Assertions; 
with Crypto.Types.Big_Numbers;
with Big_Number_Constants; 
with Big_Numbers_Mod_Utils; 
with Ada.Text_IO; 

pragma Elaborate_All(Crypto.Types.Big_Numbers);
package body Test.Big_Number_Div_Mod_Utils is

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

   	   X := Div(X_4096, X_4095, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 2812 Bit.");

   	   X := Div(X_4096, X_1025, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_4096);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 2048 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_4096);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_4096);
	   Assert(X = X_3812*2, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_4096);
	   Assert(X = X_2048*2, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_4096);
	   Assert(X = X_1025*2, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_4096);
	   Assert(X = X_1024*2, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_4096);
	   Assert(X = X_768*2, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_4096);
	   Assert(X = X_1*2, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_4096);
	   Assert(X = X_0*2, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_4096);
	   N := To_Big_Unsigned("290480299768497903142975126665228718534348758818" &
	   "144761833074307614360186549855511286866802226655920362566307887749025" &
	   "872199526479727002356083144283609351620051605581985322024942202492549" &
	   "452581360012238290352090619736484027001205241398829218469076114618060" &
	   "438952238494637161287586903848978440565478956275566654662175977689240" &
	   "815319079008093010012374628422407512125765222478859380206821436929049" &
	   "508627578696707312791518320295750043482186602660928341627264555395186" &
	   "141581706929979320334516297986259372358452977040250615510481950587537" &
	   "438000854768036711747287870813649742800665430847926497915233881850959" &
	   "079704426417253064293194913588172864744177331943977715580772322316509" &
	   "962719117000814602854537558776694408095949364779576576834935064613384" &
	   "273275871895789541157742231739013005144585901624769803752094974275690" &
	   "556348865373948453742852185535807506065796101227837962061950657645985" &
	   "5478234203189721457470807178553957231283664846848");
	   Assert(X = N, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_4096);
	   Assert(X = X_4095, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_4096);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_4096);
	   N := To_Big_Unsigned("810701595332144571209863258893548010853903342980" &
	   "076273868613523377290636612158917898065935782634980900262516861836343" &
	   "613283072542141493169485006271482258120088235229627606124681613417142" &
	   "124715838622111055574368800436804054407236282054324107587454901970695" &
	   "805847397653283670635258745188776577318968814061006601512903437047766" &
	   "810400248702873792835210251392221986288202081748150073968074115195547" &
	   "058748271271358756617348220498505588172217327478208646341146227507952" &
	   "953541234773714401709790817631503925470376499569033968229348629606787" &
	   "974573913553147343253160207633859418366468779957306778158210884842161" &
	   "728476602089749897764326175375320843367060105474966556443703856578822" &
	   "278356391108399209101684632478666921264325578237007211395864671983422" &
	   "611570218003029449694672688596549060967732916258088093936110897044509" &
	   "085169705082358357169047102671001517375500415495799705565622098851908" &
	   "477425373475247476695816173207094646324686315549779338874720457945097" &
	   "485103839157257085331571970157991017627363805190635217897694444165578" &
	   "434068257361792120182453893169724671475764576257102053067063529421319" &
	   "802491648501108299505772141778716212149809153565269436758365059447309" &
	   "701707839972186575670236313529681961579243402994113581289876835902964" &
	   "167278859938");
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_4096);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_4096);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_4096);
   	   Assert(X = X_3812, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_4096);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_4096);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_4096);
	   N := To_Big_Unsigned("104438888141315250669175271071662438257996424904" &
	   "738378038423348328395390797155745684882681193499755834089010671443926" &
	   "283798757343818579360726323608785136527794595697654370999834036159013" &
	   "438371831442807001185594622637631883939771274567233468434458661749680" &
	   "790870580370407128404874011860911446797778359802900668112937088641782" &
	   "788404612765695636622889247825646057406636521029077182128497528964197" &
	   "687949716071715263396532856325739699050620449327182551783358942800200" &
	   "894797630542196865316948980070004392046106930600417657455701720333467" &
	   "377272100921150746427522892895728406732335565748277254867426780529378" &
	   "436491306910569071326239369182600942545379992574842185534170174954795" &
	   "979786456810081907105905634134064624681871144097852818454075322045911" &
	   "314678134639298496172581572323036775263874568020514815945699459834706" &
	   "989616372825638259577805399037221463567068751821420567024999667268006" &
	   "380974195468384719651164374361186430882760858164723676185594491603344" &
	   "994875390578639227403251870096035781504383194090982048937460531727018" &
	   "285907949060791764765428953425552413181959834740341606588288960668379" &
	   "519363239481101581817119426411736140440557999609778825077208806931842" &
	   "746855319096724877572482341408248476143275434954105697419219153215020" &
	   "7460048633855");
   	   Assert(X = N, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_4096);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_4096);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_4096);
   	   Assert(X = X_1*X_2048, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_4096);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_4096);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_4096);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_4096);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_4096);
   	   Assert(X = X_1025, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_4096);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_4096);
	   N := To_Big_Unsigned("104438888141315250669175271071662438257996424904" &
	   "738378038423348328395390797155745684882681193499755834089010671443926" &
	   "283798757343818579360726323608785136527794595697654370999834036159013" &
	   "438371831442807001185594622637631883939771274567233468434458661749680" &
	   "790870580370407128404874011860911446797778359802900668112937088641782" &
	   "788404612765695636622889247825646057406636521029077182128497528964197" &
	   "687949716071715263396532856325739699050620449327182551783358942800200" &
	   "894797630542196865316948980070004392046106930600417657455701720333467" &
	   "377272100921150746427522892895728406732335565748277254867426780529378" &
	   "436491306910569071326239369182600942545379992574842185534170174954795" &
	   "979786456810081907105905634134064624681871144097852818454075322045911" &
	   "314678134639298496172581572323036775263874568020514815945699459834706" &
	   "989616372825638259577805399037221463567068751821420567024999667268006" &
	   "380974195468384719651164374361186430882760858164705699254245868444267" &
	   "701823482688391891223482080672970054161375078317714468387364218456170" &
	   "553667195458679753377441814089786534205078393078092321845225013255941" &
	   "742573896994553054186897466287126728495249704401278248193393738697596" &
	   "458707927785670794848766006357180017513451440229511849447588669679387" &
	   "7835824496640");
   	   Assert(X = N, "Failed with 1024 and 1025 Bit.");
	   
	   X := Div(X_768, X_1025, X_4096);
	   N := To_Big_Unsigned("611471541926596768840496046885496155431895802289" &
	   "265304077743559425434776777152278868572973168518203973392309577160507" &
	   "918921557510807341262655315118963264315651211464819068466966941946712" &
	   "826965173367807237491789741311132634709852991173556498565106446028417" &
	   "724290497302792815513913722864095918870791828618987485350315122821902" &
	   "190119849986846345834015708247631357681278355577792407843601928676398" &
	   "759405309768600629013592457913561669956707487901893644200714410156460" &
	   "461259081142627564768834062320536800556268892432714066147085839640408" &
	   "254229772529470222847220039448397579005033712791592800273261225181600" &
	   "801106404219074586700762997014254958852261930646824393289781062556173" &
	   "386493775812035693158347403610289266460572698870861211313035751104866" &
	   "224007649314202348868715355187158911185063107277705499333000814955520" &
	   "135761854011941857274963303673258747415543901293056798833748665465409" &
	   "570739334435350215354548017696074286577012432454779437806003309215630" &
	   "323661676124003705923939730950473328753544550030698786576831744754989" &
	   "128403853379558412442141769209168903061607006467824025695392177897957" &
	   "015645937685240402298539217532821654094282700363598586258934956671631" &
	   "3600");
   	   Assert(X = N, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_4096);
   	   Assert(X = Inverse(X_1025, X_4096), "Failed with 1 and 1025 Bit.");
	   
	   X := Div(X_0, X_1025, X_4096);
   	   Assert(X = X_0, "Failed with 0 and 1025 Bit.");
	   
   end Big_Number_Mod_Utils_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := Div(X_4096, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_3812);
	   N := To_Big_Unsigned(Number_2);
	   Assert(X = N, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_3812);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 2048 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_3812);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_3812);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_3812);
	   Assert(X = (N/2 + 1), "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_3812);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_3812);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_3812);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_3812);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_3812);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_3812);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_3812);
   	   Assert(X = X_2048, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_3812);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_3812);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_3812);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_3812);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_3812);
   	   Assert(X = X_1025, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_3812);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_3812);
   	   Assert(X = X_0, "Failed with 1024 and 1025 Bit.");
	   
	   X := Div(X_768, X_1025, X_3812);
   	   Assert(X = X_0, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_3812);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");
	   
   end Big_Number_Mod_Utils_Test2;
	   
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Div(X_4096, X_4095, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_2048);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 2048 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_2048);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_2048);
	   N := To_Big_Unsigned("542581094938353831267005078077301813455332626197" &
	   "532478487032690733690574989498734956170440913380848316173117129191129" &
	   "134104433241996929519836003075270657500169941932389401558606821085306" &
	   "610944424016934770884353182822043416683189592198666071489037630050011" &
	   "276814234889528968692584380261875645637367752659122701604468991456718" &
	   "971030811701436162260194477918191839609575122850302979345829319580842" &
	   "821642196672689151417672752262418263972708533405528908778251518344371" &
	   "215935771125888195055184366286238879976644129744572859518433039459700" &
	   "515206433721552704371304297895085326990534244624842914043100866897948" &
	   "5715924823631161");
	   Assert(X = N, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_2048);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_2048);
	   N := To_Big_Unsigned("359538626972463181545861038157804946723595395788" &
	   "461314546860162315465351611001926265416954644815072042240227759742786" &
	   "715317579537628833244985694861278948248755535786849730970552604439202" &
	   "492188238906165904170011537676301364684925762947826221081654474326701" &
	   "021369172596479894491876959432609670712659248448274432");
	   Assert(X = N, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_2048);
	   Assert(X = N - 2, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_2048);
	   Assert(X = 2*X_768, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_2048);
	   Assert(X = X_1*2, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_2048);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_2048);
	   N := To_Big_Unsigned("898846567431157953864652595394512366808988489471" &
	   "153286367150405788663379027504815663542386612037680105600569399356966" &
	   "788293948844072083112464237153197370621888839467124327426381511098006" &
	   "230470597265414760425028844190753411712314407369565552704136185816752" &
	   "55342293149119973622969239858152417678164812112068608");
	   Assert(X = N, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_2048);
	   N := To_Big_Unsigned(Number_3);
	   Assert(X = N, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_2048);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_2048);
	   N := To_Big_Unsigned("313545716116086627729478127186654141665474512722" &
	   "572550344212144982049690384116493859765212965387031318510670327547022" &
	   "911332108148905896075168870205181597639098710042409795068117910729975" &
	   "689396370507895230200399406528970851812382006279755426964213805983062" &
	   "702339701015314602006372685958818824709905280092689110161076371272604" &
	   "030197909530445927510817386134560708383235035834547758902672856721019" &
	   "343551398599880597697792416257296850856249137044005918176649712137178" &
	   "650232746517893394732457771816352167688627579841201245692261515515358" &
	   "112554834380869617623111036710599143149316355114779175897318602598034" &
	   "91200942244417463");
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_2048);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_2048);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_2048);
	   N := Add(X_3812, X_0, X_2048);
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_2048);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_2048);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_2048);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_2048);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_2048);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_2048);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_2048);
   	   Assert(X = X_1025, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_2048);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_2048);
	   N := To_Big_Unsigned("323170060713110073007148766886699519604441026697" &
	   "154840321303454275246551388678908931972014115229134636887179609218980" &
	   "194941195591504909210950881523864482831206308773673009960917501977503" &
	   "896521067960576383840675682767922186426197561618380943384761704705816" &
	   "458520363050428875758915410658086075523991239303855217345640761821108" &
	   "299120442674856670214943783374320914008043755781732935169029593510175" &
	   "501155417695807048234898385904639999395613834872292819108501825938131" &
	   "068493577912859547244488548821256852293502254596285716921662078465860" &
	   "105357567416120558486100983802516247486433064053712326664724318887204" &
	   "97281435372093440");
   	   Assert(X = N, "Failed with 1024 and 1025 Bit.");
	   
	   X := Div(X_768, X_1025, X_2048);
   	   Assert(X = X_768*X_1025, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_2048);
   	   Assert(X = X_1025, "Failed with 1 and 1025 Bit.");
	   
   end Big_Number_Mod_Utils_Test3;

------------------------------------------------------------------------------------
-------------------------------------- Test 4 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test4(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin

   	   X := Div(X_4096, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_1025);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_1025);
	   Assert(X = X_1, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_1025);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_1025);
	   Assert(X = X_1, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_1025);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_1025);
	   Assert(X = X_1024, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_1025);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_1025);
	   N := To_Big_Unsigned(Number_33);
	   Assert(X = N, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_1025);
	   Assert(X = X_1, "Failed with 2048 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_1025);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_1025);
	   Assert(X = X_1, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_1025);
	   N := To_Big_Unsigned("179769313486231590772930519078902473361797697894" &
	   "230657273430081157732675805499910614616176541494793110664856993854276" &
	   "661047650716776390571540161053753143245968939246946914997545605088528" &
	   "038810652295078540604770976003582739469712568662521065991105028678000" &
	   "244329895599767364719649356344657957463482970408079366");
	   Assert(X = N, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_1025);
	   Assert(X = X_1024, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_1025);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_1025);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_1025);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_1025);
	   N := To_Big_Unsigned("402242916761047294487828918269312351101920579955" &
	   "291822157308548558645891872225994974339799968147632462547330623212263" &
	   "095545602640392472053711969162107384303308333793073846427929209233133" &
	   "397076253631753417822883473189344223927227232597379098290316096147493" &
	   "86635326728305323955347290195802100983160671546900480");
   	   Assert(X = N, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_1025);
   	   Assert(X = X_0, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_1025);
   	   Assert(X = N, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_1025);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_1025);
	   N := To_Big_Unsigned("139545021810126861324147627251971238251605639898" &
	   "701475057699226301868086618278363635274497325592772774865380817550167" &
	   "048104229504775169417121650514428735694046934514117480842483381296287" &
	   "906386494089907610302717421519216259949740158214175200711795627548601" &
	   "124049259569934623290591189520502734373168952677236736");
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_1025);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_1025);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_1025);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_1025);
   	   Assert(X = X_1, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_1025);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_1025);
   	   Assert(X = X_1024, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_1025);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_1025);
   	   Assert(X = X_1, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_1025);
	   N := To_Big_Unsigned("179769313486231590772930519078902473361797697894" &
	   "230657273430081157732675805499910614616176541494793110664856993854276" &
	   "661047650716776390571540161053753143245968939246946914997545605088528" &
	   "038810652295078540604770976003582739469712568662521065991105028678000" &
	   "244329895599767364719649356344657957463482970408079366");
	   Assert(X = N, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_1025);
   	   Assert(X = X_1024, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_1025);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_1025);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_1025);
   	   Assert(X = X_0, "Failed with 1024 and 1025 Bit.");

	   X := Div(X_768, X_1025, X_1025);
   	   Assert(X = X_0, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_1025);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

   end Big_Number_Mod_Utils_Test4;

------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
   	   
   	   X := Div(X_4096, X_4095, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_1024);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 20448 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_1024);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_1024);
	   N := To_Big_Unsigned(Number_39);
	   Assert(X = N, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_1024);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_1024);
	   Assert(X = 2, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_1024);
	   Assert(X = X_0, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_1024);
	   N := To_Big_Unsigned("210503618460156182548582091051377203423339322227" &
	   "810407605210190537275377266175681765729295590097546139426214641456693" &
	   "431600882296046958566913588574550062562278408909944441697070053270938" &
	   "1396945165052578246743293755785693307632115700");
	   Assert(X = N, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_1024);
	   Assert(X = 2, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_1024);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_1024);
	   N := To_Big_Unsigned(Number_36);
	   Assert(X = N, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_1024);
	   N := To_Big_Unsigned(Number_36);
	   Assert(X = N, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_1024);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_1024);
	   N := X_3812 - (X_3812/X_1024)*(X_1024);
   	   Assert(X = N, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_1024);
   	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_1024);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_1024);
	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_1024);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_1024);
   	   Assert(X = X_1, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 1025 Bit.");

	   X := Div(X_768, X_1025, X_1024);
   	   Assert(X = X_768, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_1024);
   	   Assert(X = X_1, "Failed with 1 and 1025 Bit.");

	   X := Div(X_0, X_1025, X_1024);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

	   X := Div(X_768, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 768 and 1024 Bit.");
	   
	   X := Div(X_1, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 1 and 1024 Bit.");

	   X := Div(X_0, X_1024, X_1024);
   	   Assert(X = X_0, "Failed with 0 and 1024 Bit.");

	   X := Div(X_1024, X_768, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 768 Bit.");

	   X := Div(X_1024, X_1, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 1 Bit.");
	   
	   X := Div(X_1024, X_0, X_1024);
   	   Assert(X = X_0, "Failed with 1024 and 1 Bit.");

   end Big_Number_Mod_Utils_Test5;

------------------------------------------------------------------------------------
-------------------------------------- Test 6 --------------------------------------
------------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test6(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions; 
   begin
  	   
   	   X := Div(X_4096, X_4095, X_768);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_768);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_768);
	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_768);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_768);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_768);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_768);
	   N := To_Big_Unsigned("255041712325214193543620134034560633607854808310" &
	   "227263657137961617245492663224814851705276909178074099079340528299213" &
	   "542657031324674157075733464624343452806649944233433494310977742377880" &
	   "363559402774734944295153370111715521120314485");
	   Assert(X = N, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_768);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_768);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_768);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_768);
	   Assert(X = X_0, "Failed with 20448 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_768);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_768);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_768);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_768);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_768);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_768);
	   Assert(X = X_0, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_768);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_768);
	   Assert(X = X_0, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_768);
	   Assert(X = X_0, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_768);
	   Assert(X = X_0, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_768);
	   Assert(X = X_0, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_768);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_768);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_768);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_768);
	   Assert(X = X_0, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_768);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_768);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_768);
	   N := To_Big_Unsigned("653779902312997553143265294645723325362275709724" &
	   "639650841594457151811189497051611840175877429832902398105206867791340" &
	   "350330721402454474955150703748546882809020994391577851398164004366285" &
	   "531015992650512033833400124002281087468186168");
	   Assert(X = N, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_768);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_768);
	   N := To_Big_Unsigned("496707020333810049062642925428957458050554077994" &
	   "906343246397163767590294167997797535427231067182518343726839396663095" &
	   "711410719216082419784216591210830588740717515264299688627864312218496" &
	   "540275580129422906358816427736140672116684648");
	   Assert(X = N, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_768);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_768);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_768);
	   N := To_Big_Unsigned("112664744471584677225938129586205668494197374690" &
	   "212901292765987961889882222856085379080841386856082340962864556318255" &
	   "782208737809837389505357339396771357377050361175953878659945365108622" &
	   "144692663107148359434403928614832097972904505");
   	   Assert(X = N, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_768);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_768);
	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_768);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_768);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_768);
	   N := To_Big_Unsigned("796221178997173661366251707628419603795546577374" &
	   "431290014127917568143691740249615653841318960004447426503224609324023" &
	   "829492141520446864116881459708567334514565859309919539219781918458112" &
	   "962225302865399326634971085581886641495613316");
   	   Assert(X = N, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_768);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 1025 Bit.");

	   X := Div(X_768, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

	   X := Div(X_0, X_1025, X_768);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

	   X := Div(X_768, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 768 and 1024 Bit.");
	   
	   X := Div(X_1, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 1 and 1024 Bit.");

	   X := Div(X_0, X_1024, X_768);
   	   Assert(X = X_0, "Failed with 0 and 1024 Bit.");

	   X := Div(X_1024, X_768, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 768 Bit.");

	   X := Div(X_1024, X_1, X_768);
	   N := To_Big_Unsigned("796221178997173661366251707628419603795546577374" &
	   "431290014127917568143691740249615653841318960004447426503224609324023" &
	   "829492141520446864116881459708567334514565859309919539219781918458112" &
	   "962225302865399326634971085581886641495613315");
   	   Assert(X = N, "Failed with 1024 and 1 Bit.");
	   
	   X := Div(X_1024, X_0, X_768);
   	   Assert(X = X_0, "Failed with 1024 and 0 Bit.");

   end Big_Number_Mod_Utils_Test6;

----------------------------------------------------------------------------------
------------------------------------ Test 7 --------------------------------------
----------------------------------------------------------------------------------

   procedure Big_Number_Mod_Utils_Test7(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   
   	   X := Div(X_4096, X_4095, X_1);
	   Assert(X = X_0, "Failed with 4096 and 4095 Bit.");

   	   X := Div(X_4096, X_3812, X_1);
	   Assert(X = X_0, "Failed with 4096 and 3812 Bit.");

   	   X := Div(X_4096, X_2048, X_1);
	   Assert(X = X_0, "Failed with 4096 and 2048 Bit.");

   	   X := Div(X_4096, X_1025, X_1);
	   Assert(X = X_0, "Failed with 4096 and 1025 Bit.");

   	   X := Div(X_4096, X_1024, X_1);
	   Assert(X = X_0, "Failed with 4096 and 1024 Bit.");

   	   X := Div(X_4096, X_768, X_1);
	   Assert(X = X_0, "Failed with 4096 and 768 Bit.");

   	   X := Div(X_4096, X_1, X_1);
	   Assert(X = X_0, "Failed with 4096 and 1 Bit.");

   	   X := Div(X_4096, X_0, X_1);
	   Assert(X = X_0, "Failed with 4096 and 0 Bit.");

   	   X := Div(X_4095, X_4096, X_1);
	   Assert(X = X_0, "Failed with 4095 and 4096 Bit.");

   	   X := Div(X_3812, X_4096, X_1);
	   Assert(X = X_0, "Failed with 3812 and 4096 Bit.");

   	   X := Div(X_2048, X_4096, X_1);
	   Assert(X = X_0, "Failed with 20448 and 4096 Bit.");

   	   X := Div(X_1025, X_4096, X_1);
	   Assert(X = X_0, "Failed with 1025 and 4096 Bit.");

   	   X := Div(X_1024, X_4096, X_1);
	   Assert(X = X_0, "Failed with 1024 and 4096 Bit.");

   	   X := Div(X_768, X_4096, X_1);
	   Assert(X = X_0, "Failed with 768 and 4096 Bit.");

   	   X := Div(X_1, X_4096, X_1);
	   Assert(X = X_0, "Failed with 1 and 4096 Bit.");

   	   X := Div(X_0, X_4096, X_1);
	   Assert(X = X_0, "Failed with 0 and 4096 Bit.");

   	   X := Div(X_3812, X_4095, X_1);
	   Assert(X = X_0, "Failed with 3812 and 4095 Bit.");

   	   X := Div(X_2048, X_4095, X_1);
	   Assert(X = X_0, "Failed with 2048 and 4095 Bit.");

   	   X := Div(X_1025, X_4095, X_1);
	   Assert(X = X_0, "Failed with 1025 and 4095 Bit.");

   	   X := Div(X_1024, X_4095, X_1);
	   Assert(X = X_0, "Failed with 1024 and 4095 Bit.");

   	   X := Div(X_768, X_4095, X_1);
	   Assert(X = X_0, "Failed with 768 and 4095 Bit.");

   	   X := Div(X_1, X_4095, X_1);
	   Assert(X = X_0, "Failed with 1 and 4095 Bit.");

   	   X := Div(X_0, X_4095, X_1);
	   Assert(X = X_0, "Failed with 0 and 4095 Bit.");

   	   X := Div(X_4095, X_3812, X_1);
	   Assert(X = X_0, "Failed with 4095 and 3812 Bit.");

   	   X := Div(X_4095, X_2048, X_1);
	   Assert(X = X_0, "Failed with 4095 and 2048 Bit.");

   	   X := Div(X_4095, X_1025, X_1);
	   Assert(X = X_0, "Failed with 4095 and 1025 Bit.");

   	   X := Div(X_4095, X_1024, X_1);
	   Assert(X = X_0, "Failed with 4095 and 1024 Bit.");

   	   X := Div(X_4095, X_768, X_1);
	   Assert(X = X_0, "Failed with 4095 and 768 Bit.");

   	   X := Div(X_4095, X_1, X_1);
	   Assert(X = X_0, "Failed with 4095 and 1 Bit.");

   	   X := Div(X_4095, X_0, X_1);
	   Assert(X = X_0, "Failed with 4095 and 0 Bit.");

	   X := Div(X_3812, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 2048 Bit.");
	   
	   X := Div(X_3812, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 1025 Bit.");
	   
	   X := Div(X_3812, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 1024 Bit.");
	   
	   X := Div(X_3812, X_768, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 768 Bit.");
	   
	   X := Div(X_3812, X_1, X_1);
	   Assert(X = X_0, "Failed with 3812 and 1 Bit.");
	   
	   X := Div(X_3812, X_0, X_1);
   	   Assert(X = X_0, "Failed with 3812 and 0 Bit.");
	   
	   X := Div(X_2048, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 3812 Bit.");
	   
	   X := Div(X_1025, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 3812 Bit.");
	   
	   X := Div(X_1024, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 3812 Bit.");
	   
	   X := Div(X_768, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 768 and 3812 Bit.");
	   
	   X := Div(X_1, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 1 and 3812 Bit.");
	   
	   X := Div(X_0, X_3812, X_1);
   	   Assert(X = X_0, "Failed with 0 and 3812 Bit.");
	   
	   X := Div(X_2048, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1025 Bit.");
	   
	   X := Div(X_2048, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1024 Bit.");
	   
	   X := Div(X_2048, X_768, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 768 Bit.");
	   
	   X := Div(X_2048, X_1, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 1 Bit.");
	   
	   X := Div(X_2048, X_0, X_1);
   	   Assert(X = X_0, "Failed with 2048 and 0 Bit.");
	   
	   X := Div(X_1025, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 2048 Bit.");
	   
	   X := Div(X_1024, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 2048 Bit.");
	   
	   X := Div(X_768, X_2048, X_1);
	   Assert(X = X_0, "Failed with 768 and 2048 Bit.");
	   
	   X := Div(X_1, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 1 and 2048 Bit.");
	   
	   X := Div(X_0, X_2048, X_1);
   	   Assert(X = X_0, "Failed with 0 and 2048 Bit.");
	   
	   X := Div(X_1025, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 1024 Bit.");
	   
	   X := Div(X_1025, X_768, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 768 Bit.");
	   
	   X := Div(X_1025, X_1, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 1 Bit.");
	   
	   X := Div(X_1025, X_0, X_1);
   	   Assert(X = X_0, "Failed with 1025 and 0 Bit.");
	   
	   X := Div(X_1024, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 1024 and 1025 Bit.");

	   X := Div(X_768, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 768 and 1025 Bit.");
	   
	   X := Div(X_1, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

	   X := Div(X_0, X_1025, X_1);
   	   Assert(X = X_0, "Failed with 1 and 1025 Bit.");

	   X := Div(X_768, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 768 and 1024 Bit.");
	   
	   X := Div(X_1, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 1 and 1024 Bit.");

	   X := Div(X_0, X_1024, X_1);
   	   Assert(X = X_0, "Failed with 0 and 1024 Bit.");

   end Big_Number_Mod_Utils_Test7;

----------------------------------------------------------------------------------

end Test.Big_Number_Div_Mod_Utils;
