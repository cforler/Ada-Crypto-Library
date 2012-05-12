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

with Crypto.Symmetric.Hashfunction_SHA1, Ada.Text_Io;
use  Ada.Text_Io;
  
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Crypto.Asymmetric.DSA is
   package SHA1 renames Crypto.Symmetric.Hashfunction_SHA1;
   use Big.Mod_Utils;
   use Big.Utils;
   --   use Big;
   --  package WIO is new Ada.Text_IO.Modular_IO (Word);

   ---------------------------------------------------------------------------

   -- check if key is initialited
   function Is_Init(Key : DSA_KEY) return Boolean is
   begin
      if Key.P /= Big_Unsigned_Zero and Key.Q /= Big_Unsigned_Zero and
         Key.G /= Big_Unsigned_Zero and Key.K /= Big_Unsigned_Zero then
         return True;
      else return False;
      end if;
   end Is_Init; Pragma Inline (Is_Init);

   ---------------------------------------------------------------------------

   function Check(Key : DSA_KEY) return Boolean is
   begin
      if (Bit_Length(Key.Q) /= 160) or --2**159 <Q<2**160
         (Bit_Length(Key.P) /= Size) or --2**Size-1<P< 2**Size
        (Key.G >= Key.P) or (Key.G < Big_Unsigned_Two) or -- 1<G< P
        (Key.K = Big_Unsigned_Zero) then return False;
      elsif  Key.P mod Shift_Left(Key.Q,1) /= Big_Unsigned_One then
         return False;
      elsif Looks_Like_A_Prime(Key.Q) and Looks_Like_A_Prime(Key.P) then
         return True;
      else return False;
      end if;
   end Check; pragma  Inline(Check);

   ---------------------------------------------------------------------------

   -- this function computes a 160-bit prime Q and a Size-bit prime P
   --  postcondition:   P = 1 (mod 2Q)

   -- Basic-Algorithm:
   -- 1. choose a 160-Bit Prime Q
   -- 2. chosse a random 512-bit number W
   -- 3. compute C <-- W mod 2Q
   -- 4. compute P <-- W-(C-1)  -- P = 1 mod 2Q
   -- 5. if P is prime return
   -- 6. compute W <-- W + 2Q
   -- 7. goto 4

   procedure Get_PQ(P,Q : out Big_Unsigned;
                    OK : out boolean) is
      Bit_Length_Of_Q : constant := 160;
      Max_Counter_Length : constant := 4096;
      W : Big_Unsigned := Get_Random(Big_Unsigned_Last);
      C, Q_2 : Big_Unsigned;
   begin
      --1
      Q := Get_N_Bit_Prime(Bit_Length_Of_Q);
      Q_2 := Shift_Left(Q,1); -- 2Q

      --2
      -- Make sure that W is a odd Size-Bit number
      Set_Least_Significant_Bit(W);
      Set_Most_Significant_Bit(W);

      --3
      C := W mod Q_2;

      for I in 0..Max_Counter_Length  loop
         --4
         P := W-(C-1); -- P = 1 mod 2Q
         if Bit_Length(P) = Size then
            if Is_Prime(P) then
               --5
               Ok := True;
               return;
            end if;
         else
            OK:=False;
            return;
         end if;

         --6
         W := W + Q_2;
         if Bit_Length(W) /= Size then exit;
         end if;
         --7
      end loop;

      -- No prim P and Q found with P = 1 (mod 2Q)

      OK:=False;
      return;
   end Get_PQ; pragma Inline (Get_PQ);

   ---------------------------------------------------------------------------

   procedure Primgen(P,Q : out Big_Unsigned) is
      Ok : Boolean;
   begin
      loop
         Get_PQ( P, Q, OK);
         if Ok then return;
         end if;
      end loop;
   end primgen;

   ---------------------------------------------------------------------------

   -- based on the algorithm in fips-186-2 page 8.

   procedure Gen_Key(Public_Key  : out Public_Key_DSA;
                     Private_Key : out Private_Key_DSA) is

      H : Big_Unsigned := Big_Unsigned_Two;

   begin
      -- DSA Parameters (fips 186-2)
      --1 & 2

      Primgen(Public_Key.P, Public_Key.Q);

      Private_Key.P := Public_Key.P;
      Private_Key.Q := Public_Key.Q;

      --3
      loop
         Public_Key.G := Pow(H, (Public_Key.P-1)/Public_Key.Q, Public_Key.P);
         if Public_Key.G > Big_Unsigned_One then exit;
         else
            Inc(H);
         end if;
         if H >= Public_Key.P then raise Constraint_Error;
         end if;
      end loop;
      Private_Key.G := Public_Key.G;

      --4
      while Private_Key.K =  Big_Unsigned_Zero loop
         Private_Key.K := Get_Random(Private_Key.Q);
      end loop;

      --5
      Public_Key.K := Pow(Private_Key.G, Private_Key.K, Private_Key.P);
   end Gen_Key;

   ---------------------------------------------------------------------------

   procedure Sign(Private_Key : in  Private_Key_DSA;
                  SHA1_Hash   : in  W_Block160;
                  Signature   : out Signature_DSA) is
      K : Big_Unsigned;
      H : constant Big_Unsigned := To_Big_Unsigned(SHA1.To_Bytes(Sha1_Hash));
   begin
      loop
         loop
            K := Get_Random(Private_Key.Q);
            exit when  K /= Big_Unsigned_Zero;
         end loop;
	 
         Signature.R := Pow(Private_Key.G, K, Private_Key.P) mod Private_Key.Q;
	 
         Signature.S :=
           Mult(Inverse(K, Private_Key.Q),
                Add(H, Mult(Private_Key.K, Signature.R, Private_Key.Q),
                    Private_Key.Q), Private_Key.Q);
	 
         exit when Signature.R /= Big_Unsigned_Zero and
           Signature.S /= Big_Unsigned_Zero;
      end loop;
   end Sign;

   ---------------------------------------------------------------------------

   function Verify(Public_Key  : Public_Key_DSA;
                   SHA1_Hash   : W_Block160;
                   Signature   : Signature_DSA) return Boolean is


       W  : constant Big_Unsigned := Inverse(Signature.S, Public_Key.Q);

       U1 : constant Big_Unsigned :=
        Mult(To_Big_Unsigned(SHA1.To_Bytes(Sha1_Hash)), W,Public_Key.Q);

       U2 : constant Big_Unsigned := Mult(Signature.R, W, Public_Key.Q);

       V : constant Big_Unsigned:= Mult(Pow(Public_Key.G, U1, Public_Key.P),
					Pow(Public_Key.K, U2, Public_Key.P),
					Public_Key.P) mod Public_Key.Q;
   begin
      if V = Signature.R then return True;
      else return False;
      end if;
   end Verify;

   ---------------------------------------------------------------------------

   procedure Set_Public_Key(P : in DSA_Number;
                            Q : in DSA_Number;
                            G : in DSA_Number;
                            Y : in DSA_Number;
                            Public_Key : out Public_Key_DSA) is

   begin
      Public_Key.P := To_Big_Unsigned(P);
      Public_Key.Q := To_Big_Unsigned(Q);
      Public_Key.G := To_Big_Unsigned(G);
      Public_Key.K := To_Big_Unsigned(Y);

      if Check(DSA_KEY(Public_Key)) = False then
         raise Invalid_Public_Key_Error;
      end if;
   end Set_Public_Key;

   ---------------------------------------------------------------------------

   procedure Set_Private_Key(P : in DSA_Number;
                             Q : in DSA_Number;
                             G : in DSA_Number;
                             X : in DSA_Number;
                             Private_Key : out Private_Key_DSA) is
   begin
      Private_Key.P := To_Big_Unsigned(P);
      Private_Key.Q := To_Big_Unsigned(Q);
      Private_Key.G := To_Big_Unsigned(G);
      Private_Key.K := To_Big_Unsigned(X);

      if Check(DSA_KEY(Private_Key)) = False then
         raise Invalid_Private_Key_Error;
      end if;
   end Set_Private_Key;

   ---------------------------------------------------------------------------

   procedure Get_Public_Key(Public_Key : in Public_Key_DSA;
                            P : out DSA_Number;
                            Q : out DSA_Number;
                            G : out DSA_Number;
                            Y : out DSA_Number) is
   begin
      P := To_Bytes(Public_Key.P);

      Q := (others => 0);
      Q(Q'Last-(B_Block160'Length-1)..Q'Last)
        := To_Bytes(Public_Key.Q);

      G := (others =>0);
      G(G'Last-(Length_In_Bytes(Public_Key.G)-1)..G'Last)
        := To_Bytes(Public_Key.G);

      Y := (others =>0);
      Y(Y'Last-(Length_In_Bytes(Public_Key.K)-1)..Y'Last)
        := To_Bytes(Public_Key.K);
   end Get_Public_Key;

   ---------------------------------------------------------------------------

 procedure Get_Private_Key(Private_Key : in Private_Key_DSA;
                            P : out DSA_Number;
                            Q : out DSA_Number;
                            G : out DSA_Number;
                            X : out DSA_Number) is
   begin
      P := To_Bytes(Private_Key.P);

      Q := (others => 0);
      Q(Q'Last-(B_Block160'Length-1)..Q'Last) :=
        To_Bytes(Private_Key.Q);

      G := (others =>0);
      G(G'Last-(Length_In_Bytes(Private_Key.G)-1)..G'Last)
        := To_Bytes(Private_Key.G);

      X := (others =>0);
      X(X'Last-(Length_In_Bytes(Private_Key.K)-1)..X'Last)
        := To_Bytes(Private_Key.K);
   end Get_Private_Key;

   ---------------------------------------------------------------------------

   procedure Get_Signature(Signature : in Signature_DSA;
                           R         : out Big_Unsigned;
                           S         : out Big_Unsigned)
   is
   begin
      R := Signature.R;
      S := Signature.S;
   end;

   ---------------------------------------------------------------------------

   procedure Set_Signature(R         : in Big_Unsigned; 
                           S         : in Big_Unsigned;
                           Signature : out Signature_DSA)
   is
   begin
      Signature.R := R;
      Signature.S := S;
   end;

   ---------------------------------------------------------------------------

   procedure Sign_File(Filename    : in  String;
                       Private_Key : in  Private_Key_DSA;
                       Signature   : out Signature_DSA) is
   begin
      if Is_Init(DSA_Key(Private_Key)) then
         Sign(Private_Key, SHA1.F_Hash(Filename), Signature);
      else
         raise Invalid_Private_Key_Error;
      end if;
   end Sign_File;

   ---------------------------------------------------------------------------

   function Verify_File(Filename   : String;
                        Public_Key : Public_Key_DSA;
                        Signature  : Signature_DSA) return Boolean is
   begin
      if Is_Init(DSA_Key(Public_Key)) then
         return Verify(Public_Key, SHA1.F_Hash(Filename), Signature);
      else
         raise Invalid_Public_Key_Error;
      end if;
   end Verify_File;

   ---------------------------------------------------------------------------

   function Verify_Key_Pair(Private_Key : Private_Key_DSA;
                            Public_Key  : Public_Key_DSA) return Boolean is
   begin
      if Check(DSA_KEY(Public_Key)) = False or
        Check(DSA_KEY(Private_Key)) = False then
         return False;
      elsif
        (Private_Key.G = Public_Key.G) and
        (Private_Key.P = Public_Key.P) and
        (Private_Key.Q = Public_Key.Q) and
        (Pow(Private_Key.G, Private_Key.K, Private_Key.P) = Public_Key.K) then
         return True;
      else return False;
      end if;
   end Verify_Key_Pair;

   ---------------------------------------------------------------------------

begin
   declare
      Pass : Boolean:=False;
   begin
      for I in 0..8  loop
         if Size = 512+64*I then
            Pass:=True;
            exit;
         end if;
      end loop;

      if Pass = False then
         Put("DSA is only specified for L=512+64*j-bit primes with ");
         Put_Line("512 <= L <= 1024");
         raise Constraint_Size_Error;
      end if;
   end;
end Crypto.Asymmetric.DSA;
