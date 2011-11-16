
-- non-supersingular elliptic curves over binary fields

generic
package Crypto.Types.Elliptic_Curves.NSS_BF is
   use Big;

      -- init an elliptic curve over GF(2**deg(F))
   procedure Init(A, B, F : in Big_Unsigned);


   function Is_Elliptic_Curve return Boolean;

   function On_Elliptic_Curve(X : EC_Point) return Boolean;

   function Negative(X : EC_Point) return EC_Point;

   function "+"(Left, Right : EC_Point) return EC_Point;

   function "-"(Left, Right : EC_Point) return EC_Point;

   function Double(X : EC_Point) return EC_Point;

   function "*"(Left : Big_Unsigned; Right : EC_Point) return EC_Point;

end Crypto.Types.Elliptic_Curves.NSS_BF;
