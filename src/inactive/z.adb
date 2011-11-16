
with Ada.Text_IO;
with Crypto.Types;
with Crypto.Types.Elliptic_Curves.Zp;
with Crypto.Types.Big_Numbers;
use Ada.Text_IO;

procedure Z is
   package Big is new Crypto.Types.Big_Numbers(128);
   package EC  is new Crypto.Types.Elliptic_Curves(Big);
   use Big;
   use EC;

--   A : Big.Big_Unsigned := Big_Unsigned_Four;
--   B : Big.Big_Unsigned := Big_Unsigned_Zero + 20;
--   P : Big.Big_Unsigned := Big_Unsigned_zero + 29;


--   K : Big.Big_Unsigned;
--   P1 : EC_Point := (Big_Unsigned_One, A+1);


   package EC_Zp is new EC.Zp;
   use EC_Zp;

begin
   Init(30);

   Put_Line(Is_Elliptic_Curve'Img);

end Z;
