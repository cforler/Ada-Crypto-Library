
package body Crypto.Types.Elliptic_Curves.SS_BF is
   use Big.Binfield_Utils;

   -- y^2 + cy = x^3 + ax + b mod F

   A : Big_Unsigned;
   B : Big_Unsigned;
   C : Big_Unsigned;

   -- irreducible polynom F that creates the binary field GF(2^deg(F))
   F : Big_Unsigned;

   ---------------------------------------------------------------------------

   -- init a ec over Z_p
   procedure Init(A, B, C, F : in Big_Unsigned) is
   begin
      SS_BF.A := A;
      SS_BF.B := B;
      SS_BF.C := C;
      SS_BF.F := F;
   end init ;

 ---------------------------------------------------------------------------

   -- test if y^2 + cy = x^3 + ax + b mod p is a valid elliptic curve
   -- functionality:
   -- 1. Test if F is irreducibel TODO
   -- 2. compute the discriminante  D of the elliptic curve
   -- 3. Test if D /= 0
   function Is_Elliptic_Curve return Boolean is
      D : Big_Unsigned := Big_Unsigned_Zero + 27;
   begin
      D := B_Mult(D,B_Square(B_Add(B_Square(C,F),
                                   B_Mult(Big_Unsigned_Four,B,F)),F),F);

      D := B_Inverse(B_Add(D, B_Mult(Big.Utils.Shift_Left(Big_Unsigned_One,6),
                                     B_Mult( B_Square(A,F) ,A,F) ,F)),F);
      if D /= Big_Unsigned_Zero then
         return True;
      else
         return False;
      end if;
   end Is_Elliptic_Curve;

   ---------------------------------------------------------------------------

   function On_Elliptic_Curve(X : EC_Point) return Boolean is
   begin
      if X = EC_Point_Infinity then
         return True;
      else
         if B_Add(B_Square(X.Y,F), B_Mult(C, X.Y, F)) =
           B_Add(B_Mult(B_Square(X.X,F),X.X,F), B_Add(B_Mult(A, X.X, F), B))
         then
            return True;
         else
            return False;
         end if;
      end if;
   end On_Elliptic_Curve;


   ---------------------------------------------------------------------------

    function Negative(X : EC_Point) return EC_Point is
       Result : EC_Point := X;
    begin
       Result.Y := B_Add(X.Y,C);
       return Result;
    end Negative;

    ---------------------------------------------------------------------------

    function "+"(Left, Right : EC_Point) return EC_Point is
    begin
      if Left = Right then
         return Double(Left);
      else
         if Left = Negative(Right) then
            return  EC_Point_Infinity;
         else
            if Left = EC_Point_Infinity then
               return Right;
            else
               if Right = EC_Point_Infinity then
                  return Left;
               end if;
            end if;
         end if;
      end if;

      declare
         Result : EC_Point;
         Temp : constant Big_Unsigned :=
           B_Div(B_Add(Left.X, Right.X), B_Add(Left.X, Right.X),F);
      begin
         Result.X := B_Add(B_Square(Temp,F), B_Add(Left.X,Right.X));
         Result.Y := B_Add(B_Mult(Temp,B_Add(Left.X,Result.X),F),
                           B_Add(Left.Y,C));
         return Result;
      end;
    end "+";

    ---------------------------------------------------------------------------

   function "-"(Left, Right : EC_Point) return EC_Point is
   begin
      return Left + Negative(Right);
   end; pragma Inline("-");

    ---------------------------------------------------------------------------

    function Double(X : EC_Point) return EC_Point is
    begin
       if X = Negative(X) then
          return  EC_Point_Infinity;
       else
          declare
             Result : EC_Point;
             Temp : constant Big_Unsigned :=
	       B_Div(B_Add(B_Square(X.X, F), A), C, F);
          begin
             Result.X := B_Square(Temp, F);
             Result.Y := B_Add(B_Mult(Temp, B_Add(X.X, Result.X),F),
                               B_Add(X.Y,C));
             return Result;
          end;
       end if;
    end Double;

    ---------------------------------------------------------------------------


   function "*"(Left : Big_Unsigned; Right : EC_Point) return EC_Point is
      Q : EC_Point;
      P : EC_Point  := Right;
      K : constant Mod_Types :=  Big.Utils.To_Mod_Types(Left);
   begin
      if Left = Big_Unsigned_Zero then
         return EC_Point_Infinity;
      end if;

      for I in K'First..K'Last-1 loop
         for J in 0..Mod_Type'Size-1 loop
            if (Shift_Right(K(I),J) and 1) = 1  then
               Q := Q + P;
            end if;
            P := Double(P);
         end loop;
      end loop;

      declare
         L : Natural;
         Y : constant Mod_Type := K(K'Last);
      begin
         for I  in reverse 0..Mod_Type'Size-1 loop
            if  (Shift_Right(Y,I) and 1) = 1  then
               L := I;
               exit;
            end if;
         end loop;

         for I in 0..L loop
            if (Shift_Right(Y,I) and 1) = 1  then
               Q :=  Q + P;
            end if;
            P := Double(P);
         end loop;
      end;

      return Q;
   end "*";


end Crypto.Types.Elliptic_Curves.SS_BF;
