with AUnit;
with AUnit.Test_Cases;


package Test.Catena is
	use AUnit;

	type Catena_Test is new Test_Cases.Test_Case with null record;

	procedure Register_Tests(T: in out Catena_Test);

	function Name(T: Catena_Test) return Test_String;

	procedure Catena_Test_XORShift(T: in out Test_Cases.Test_Case'Class);
 	procedure Catena_Test_Bitreverse(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_CFAES(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_Interface(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_SHA512(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_B2b1(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_B2b1Bla(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_GR(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_Pcompress(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_ArgonCF(T: in out Test_Cases.Test_Case'Class);
  procedure Catena_Test_Keyed(T: in out Test_Cases.Test_Case'Class);
      
      -- procedure Catena_Test_Debugging(T: in out Test_Cases.Test_Case'Class);
      -- procedure Catena_Test_Flap_DBG(T: in out Test_Cases.Test_Case'Class);
    --   procedure Catena_Test_Flap_BRG(T: in out Test_Cases.Test_Case'Class);
      -- procedure Catena_Test_Catena_BRG(T: in out Test_Cases.Test_Case'Class);
    --   procedure Catena_Test_Catena_DBG(T: in out Test_Cases.Test_Case'Class);
      -- procedure Catena_Test_HashFast(T: in out Test_Cases.Test_Case'Class);
end Test.Catena;
