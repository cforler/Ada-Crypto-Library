-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

-- With this packet you can generate (unsigned) n-Bit Numbers (Big_Unsigned).
-- You can only create k*m Bit-Numbers where 1 < k < 2**32 and  m is the
-- size of a CPU-Word. For further informations read the ACL documentation.

-- The look and feel is "borrowed" from  J. Delcourt BIG_NUMBER package.
-- First I want to use Delcourts package directly, but then I decided to
-- rewrite it completly form scratch. ;-)

with System;

with Crypto.Types;
use Crypto.Types;

generic
   Size : Positive;
   Rand_Source : String := "";

package Crypto.Types.Big_Numbers is

   type Big_Unsigned is private;
   subtype Number_Base is Integer range 2 .. 16;

   -- Do not use this type. This one is only needed for internal purpose.
    type D_Big_Unsigned is private;

   ---------------------------------------------------------------------------
   ---------------------------Constants---------------------------------------
   ---------------------------------------------------------------------------

   -- A few constants
   Big_Unsigned_Zero    : constant Big_Unsigned; -- 0
   Big_Unsigned_One     : constant Big_Unsigned; -- 1
   Big_Unsigned_Two     : constant Big_Unsigned; -- 2
   Big_Unsigned_Three   : constant Big_Unsigned; -- 3
   Big_Unsigned_Four    : constant Big_Unsigned; -- 4
   Big_Unsigned_Ten     : constant Big_Unsigned; -- 10
   Big_Unsigned_Sixteen : constant Big_Unsigned; -- 16
   Big_Unsigned_First   : constant Big_Unsigned; -- 0
   Big_Unsigned_Last    : constant Big_Unsigned; -- "Big_Unsigned'Last"

   ---------------------------------------------------------------------------
   ----------------------------Compare----------------------------------------
   ---------------------------------------------------------------------------

   -- compare: Big Unsigned with Big_Unsigned
   function "="(Left, Right : Big_Unsigned) return Boolean;
   function "<"(Left, Right : Big_Unsigned) return Boolean;
   function ">"(Left, Right : Big_Unsigned) return Boolean;

   function "<="(Left, Right : Big_Unsigned) return Boolean;
   function ">="(Left, Right : Big_Unsigned) return Boolean;

   function Min(X, Y : in Big_Unsigned) return Big_Unsigned;
   function Max(X, Y : in Big_Unsigned) return Big_Unsigned;

   -- compare: Big Unsigned with Word
   function "="(Left : Big_Unsigned; Right : Word) return Boolean;
   function "="(Left : Word; Right : Big_Unsigned) return Boolean;

   function "<"(Left : Big_Unsigned; Right : Word) return Boolean;
   function "<"(Left : Word; Right : Big_Unsigned) return Boolean;

   function ">"(Left : Big_Unsigned; Right : Word) return Boolean;
   function ">"(Left : Word; Right : Big_Unsigned) return Boolean;

   function "<="(Left : Big_Unsigned; Right : Word) return Boolean;
   function "<="(Left : Word; Right : Big_Unsigned) return Boolean;

   function ">="(Left : Big_Unsigned; Right : Word) return Boolean;
   function ">="(Left : Word; Right : Big_Unsigned) return Boolean;


   ---------------------------------------------------------------------------
   -----------------------------Basic-----------------------------------------
   ---------------------------------------------------------------------------
   
   function "+"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "+"(Left : Big_Unsigned; Right : Word) return Big_Unsigned;
   function "+"(Left : Word; Right : Big_Unsigned) return Big_Unsigned;

   function "-"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "-"(Left : Big_Unsigned; Right : Word) return Big_Unsigned;
   function "-"(Left : Word; Right : Big_Unsigned) return Big_Unsigned;

   function "-"(X : Big_Unsigned) return Big_Unsigned;

   function "*"(Left, Right : Big_Unsigned) return Big_Unsigned;
--============================================================================--
   function Russ      (Left, Right : Big_Unsigned) return Big_Unsigned;
   function Karatsuba (Left, Right : Big_Unsigned) return Big_Unsigned;
   function Karatsuba_P    (Left, Right : Big_Unsigned) return Big_Unsigned;
--   function Karatsuba_Prot (Left, Right : Big_Unsigned) return Big_Unsigned;
   function Toom_Cook      (Left, Right : Big_Unsigned) return Big_Unsigned;
   function Toom_Cook_P    (Left, Right : Big_Unsigned) return Big_Unsigned;
--============================================================================--
   function "*"(Left : Big_Unsigned; Right : Word) return Big_Unsigned;
   function "*"(Left : Word; Right : Big_Unsigned) return Big_Unsigned;

   function "/"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "/"(Left : Big_Unsigned; Right : Word) return Big_Unsigned;
   function "/"(Left : Word; Right : Big_Unsigned) return Big_Unsigned;

   function "xor"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "or" (Left, Right : Big_Unsigned) return Big_Unsigned;

   function "and"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "and"(Left: Big_Unsigned; Right: Word) return Big_Unsigned;
   function "and"(Left: Word; Right: Big_Unsigned) return Big_Unsigned;

   function "**"(Left, Right : Big_Unsigned) return Big_Unsigned;

   function "mod"(Left, Right : Big_Unsigned) return Big_Unsigned;
   function "mod"(Left : Big_Unsigned; Right : Word) return Big_Unsigned;

   ---------------------------------------------------------------------------
   ----------------------------Utils------------------------------------------
   ---------------------------------------------------------------------------

   package Utils is

      procedure Swap(X, Y : in out Big_Unsigned);

      procedure Set_Least_Significant_Bit(X : in out Big_Unsigned);
      procedure Set_Most_Significant_Bit(X : in out Big_Unsigned);

      -- Returns true if X is odd .
      function Is_Odd(X : Big_Unsigned) return Boolean;

      -- Returns true if X is even.
      function Is_Even(X : Big_Unsigned) return Boolean;


      -- Caution: All operations are mod Big_unsigned_Last+1.
      -- X = Big_unsigned_Zero
      -- Inc(X)
      -- X = Big_Unsigned_Last
      -- Dec(X)
      -- X = Big_unsigned_Zero
      procedure Inc(X : in out Big_Unsigned);
      procedure Dec(X : in out Big_Unsigned);
      
      function To_Big_Unsigned(X : Word) return Big_Unsigned;
      

      function Shift_Left(Value : Big_Unsigned; Amount : Natural)
                         return Big_Unsigned;

      function Shift_Right(Value : Big_Unsigned; Amount : Natural)
                          return Big_Unsigned;

      function Rotate_Left(Value : Big_Unsigned; Amount : Natural)
                          return Big_Unsigned;

      function Rotate_Right(Value : Big_Unsigned; Amount : Natural)
                           return Big_Unsigned;

      function Get_Random return Big_Unsigned;

      function Bit_Length(X : Big_Unsigned) return Natural;

      function Lowest_Set_Bit(X : Big_Unsigned) return Natural;

      function Length_In_Bytes(X : Big_Unsigned) return Natural;

      function Gcd(Left, Right : Big_Unsigned) return Big_Unsigned;

      function To_Bytes(X : Big_Unsigned) return Bytes;

      function To_Big_Unsigned(X : Bytes) return Big_Unsigned;

      function To_Words(X : Big_Unsigned) return Words;

      function To_Big_Unsigned(X : Words) return Big_Unsigned;

      function To_String(Item : Big_Unsigned;
                         Base : Number_Base := 10) return String;

      function To_Big_Unsigned(S : String) return Big_Unsigned;

      procedure Put(Item : in Big_Unsigned; Base : in Number_Base := 10);

      procedure Put_Line(Item : in Big_Unsigned; Base : in Number_Base := 10);


      procedure Big_Div(Dividend, Divisor : in  Big_Unsigned;
                        Quotient, Remainder   : out Big_Unsigned);

      procedure Short_Div(Dividend  : in  Big_Unsigned;
                          Divisor   : in  Word;
                          Quotient  : out Big_Unsigned;
                          Remainder : out Word);
   end Utils;

   ---------------------------------------------------------------------------
   --------------------------Mod_Utils----------------------------------------
   ---------------------------------------------------------------------------

   package Mod_Utils is
      -- All operations in this package are mod N

      function Add (Left, Right, N : Big_Unsigned) return Big_Unsigned;
      function Sub (Left, Right, N : Big_Unsigned) return Big_Unsigned;
      function Div (Left, Right, N : Big_Unsigned) return Big_Unsigned;
      function Mult(Left, Right, N : Big_Unsigned) return Big_Unsigned;
--		function Barrett(Left, Right, M : Big_Unsigned) return Big_Unsigned;
--		function Mult_School(Left, Right, N : Big_Unsigned) return Big_Unsigned;

      function Pow (Base, Exponent, N : Big_Unsigned) return Big_Unsigned;

      -- Returns a random Big_Unsigned mod N
      function Get_Random (N : Big_Unsigned) return Big_Unsigned;

      function Inverse (X, N : Big_Unsigned) return Big_Unsigned;


      -- This function returns with an overwhelming probability a prim
      function Get_Prime(N : Big_Unsigned) return Big_Unsigned;

      -- This function returns with an overwhelming probability a n-bit prim
      function Get_N_Bit_Prime(N : Positive) return Big_Unsigned;

      -- This function returns true if X is a prim and
      -- with an overwhelming probability false  if X is not prime
      -- The change that a snowball survive one day in hell are greater that
      -- this function returns true if X is no prim.
      -- functionality:
      -- 1. Test if a one digit prime (2,3,5,7) divides X
      -- 2. Test if a two digit prime number divides X
      -- 3. Test if X is a "Lucas-Lehmer" prime
      -- 4. Test if a three digit prime number divides X
      -- 5. compute N random Big_Unsigneds and test if one
      -- of those is an Miller-Rabin wittness ( 1 < N < 51)
      -- (N depends on the Bit_Length of X).
      function Is_Prime(X : Big_Unsigned) return Boolean;


      -- a weaker but faster prime test
      function Looks_Like_A_Prime(X : Big_Unsigned) return Boolean;


      -- Returns only true if X passed n iterations of the
      -- Miller-Rabin tests. This test is taken from the DSA spec (NIST FIPS
      -- 186-2).The execution time of this function is proportional
      -- to the value of this parameter.
      -- The probability that a pseudoprim pass this test is < (1/(2**(2*S)))
      function Passed_Miller_Rabin_Test(X : Big_Unsigned;
                                        S : Positive)  return Boolean;

      function Jacobi(X, N : Big_Unsigned) return Integer;


      -- internal functions for Binfield_Utils. Please,  DON'T use them.
      function Shift_Left(Value : D_Big_Unsigned; Amount : Natural)
                         return D_Big_Unsigned;
      function Bit_Length(X : D_Big_Unsigned) return Natural;
   end Mod_Utils;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   package Binfield_Utils is

      -- binary field operations
      -- F is the irreducible polynom with f(z)=2^m + r(z)
      -- Remember all operations are in GF(2^m)

        function B_Add(Left,Right : Big_Unsigned) return Big_Unsigned;
        function B_Sub(Left,Right : Big_Unsigned) return Big_Unsigned;

        function B_Mult(Left, Right, F : Big_Unsigned) return Big_Unsigned;
        function B_Div (Left, Right, F : Big_Unsigned) return Big_Unsigned;

        function B_Square(A, F : Big_Unsigned)    return Big_Unsigned;

        function B_Mod(Left, Right  : Big_Unsigned) return Big_Unsigned;

        function B_Inverse(X, F : Big_Unsigned) return Big_Unsigned;

   end Binfield_Utils;

   ---------------------------------------------------------------------------
   -----------------------------Exceptions------------------------------------
   ---------------------------------------------------------------------------

   Constraint_Size_Error : exception;
   Division_By_Zero : exception;
   Conversion_Error : exception;
   Is_Zero_Error    : exception;
   
--   Big_Unsigned_Overflow : exception;
--   Big_Unsigned_Negative : exception;

   ---------------------------------------------------------------------------
   --------------------------------PRIVATE------------------------------------
   ---------------------------------------------------------------------------

private
   type Largest_Unsigned is mod System.Max_Binary_Modulus;

   Max_Length   : Natural := (Size/Word'Size)-1;
   D_Max_Length : Positive := 2*Max_Length+1;

   subtype Limbs  is Words(0..Max_Length);
   subtype DLimbs is Words(0..D_Max_Length);

   subtype M_Len is Natural range Limbs'Range;

   -- This is our Big_Unsigned
   -- It represents a Size*Word'Size-bit number
   -- Last_Index is the Number of the last slice who
   -- contains the most significant bit of the current number.
   -- Ex.:
   -- Word'Size = 24
   -- Our Big_Unsigned A is equal to 2**100-7
   -- Big_Unsignesd_Last = 2**240-1
   -- So only Slice 0-4 contains a part of the current 99-Bit number (2**100-7)
   -- In this case A.Last_Index = 4 because A.X(5)=...=A.X(9)=0

   type Big_Unsigned is record
      Last_Index : Natural:=0;
      Number : Limbs:=(others => 0);
   end record;

   type D_Big_Unsigned is record
      Last_Index : Natural:=0;
      Number : DLimbs:=(others => 0);
   end record;

   -- prime test
   type Hardness is (Weak, Strong);


   -- Constants definitions
   Big_Unsigned_Zero : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (OTHERS => 0));
   Big_Unsigned_One : CONSTANT Big_Unsigned :=
     (Last_Index => 0,  Number => (0 => 1, OTHERS => 0));
   Big_Unsigned_Two : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (0 => 2, OTHERS => 0));
   Big_Unsigned_Three : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (0 => 3, OTHERS => 0));
   Big_Unsigned_Four : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (0 => 4, OTHERS => 0));
   Big_Unsigned_Ten : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (0 => 10, OTHERS => 0));
   Big_Unsigned_Sixteen : CONSTANT Big_Unsigned :=
     (Last_Index => 0, Number => (0 => 16, OTHERS => 0));
   Big_Unsigned_First : CONSTANT Big_Unsigned :=
     Big_Unsigned_Zero;  
   Big_Unsigned_Last : CONSTANT Big_Unsigned :=
     (Last_Index => Max_Length, Number => (OTHERS => Word'Last));


   D_Big_Unsigned_Zero : CONSTANT D_Big_Unsigned :=
     (Last_Index => 0, Number => (OTHERS => 0));
   D_Big_Unsigned_One : CONSTANT D_Big_Unsigned :=
     (Last_Index => 0,  Number => (0 => 1, OTHERS => 0));
   D_Big_Unsigned_Last : CONSTANT D_Big_Unsigned :=
     (Last_Index => D_Max_Length, Number => (OTHERS => Word'Last));

   -- Shifting

   function Shift_Left  (Value : Largest_Unsigned; Amount : Natural)
                        return Largest_Unsigned;
   function Shift_Right (Value : Largest_Unsigned; Amount : Natural)
                        return Largest_Unsigned;
   


   --pragma Inline("-",  "/", "**", "mod", "xor", "and", "or");
   pragma Inline("=", "<", ">", "<=", ">=", Min, Max);
   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);

   pragma Optimize (Time);

end Crypto.Types.Big_Numbers;
