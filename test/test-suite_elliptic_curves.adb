with Test.Elliptic_Curves_ZP;
with Test.Elliptic_Curves_NSS_BF;
with Test.ECDH;
with Test.ECIES;

package body Test.Suite_Elliptic_Curves is
	use Aunit.Test_Suites;

	Result:							aliased Test_Suite;
	Test_Elliptic_Curves_ZP:		aliased Test.Elliptic_Curves_ZP.Elliptic_Curves_Test;
	Test_Elliptic_Curves_NSS_BF:	aliased Test.Elliptic_Curves_NSS_BF.Elliptic_Curves_Test;
	Test_ECDH:				aliased Test.ECDH.ECDH_Test;
	Test_ECIES:				aliased Test.ECIES.ECIES_Test;

	function Suite return Aunit.Test_Suites.Access_Test_Suite is

	begin

		Add_Test(Result'Access, Test_Elliptic_Curves_ZP'Access);
		Add_Test(Result'Access, Test_Elliptic_Curves_NSS_BF'Access);
		Add_Test(Result'Access, Test_ECDH'Access);
		Add_Test(Result'Access, Test_ECIES'Access);

		return Result'Access;
	
	end Suite;

end Test.Suite_Elliptic_Curves;
