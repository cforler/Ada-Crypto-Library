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

-- All procedures and functions are from the fips-180-2
-- Please read this publication.

package body Crypto.Symmetric.Algorithm.Sha_Utils is

   Current_Message_Length : Message_Length128;

   K :  constant DWords(0..79) :=
     (
      16#428a2f98d728ae22#, 16#7137449123ef65cd#,
      16#b5c0fbcfec4d3b2f#, 16#e9b5dba58189dbbc#,
      16#3956c25bf348b538#, 16#59f111f1b605d019#,
      16#923f82a4af194f9b#, 16#ab1c5ed5da6d8118#,
      16#d807aa98a3030242#, 16#12835b0145706fbe#,
      16#243185be4ee4b28c#, 16#550c7dc3d5ffb4e2#,
      16#72be5d74f27b896f#, 16#80deb1fe3b1696b1#,
      16#9bdc06a725c71235#, 16#c19bf174cf692694#,
      16#e49b69c19ef14ad2#, 16#efbe4786384f25e3#,
      16#0fc19dc68b8cd5b5#, 16#240ca1cc77ac9c65#,
      16#2de92c6f592b0275#, 16#4a7484aa6ea6e483#,
      16#5cb0a9dcbd41fbd4#, 16#76f988da831153b5#,
      16#983e5152ee66dfab#, 16#a831c66d2db43210#,
      16#b00327c898fb213f#, 16#bf597fc7beef0ee4#,
      16#c6e00bf33da88fc2#, 16#d5a79147930aa725#,
      16#06ca6351e003826f#, 16#142929670a0e6e70#,
      16#27b70a8546d22ffc#, 16#2e1b21385c26c926#,
      16#4d2c6dfc5ac42aed#, 16#53380d139d95b3df#,
      16#650a73548baf63de#, 16#766a0abb3c77b2a8#,
      16#81c2c92e47edaee6#, 16#92722c851482353b#,
      16#a2bfe8a14cf10364#, 16#a81a664bbc423001#,
      16#c24b8b70d0f89791#, 16#c76c51a30654be30#,
      16#d192e819d6ef5218#, 16#d69906245565a910#,
      16#f40e35855771202a#, 16#106aa07032bbd1b8#,
      16#19a4c116b8d2d0c8#, 16#1e376c085141ab53#,
      16#2748774cdf8eeb99#, 16#34b0bcb5e19b48a8#,
      16#391c0cb3c5c95a63#, 16#4ed8aa4ae3418acb#,
      16#5b9cca4f7763e373#, 16#682e6ff3d6b2b8a3#,
      16#748f82ee5defb2fc#, 16#78a5636f43172f60#,
      16#84c87814a1f0ab72#, 16#8cc702081a6439ec#,
      16#90befffa23631e28#, 16#a4506cebde82bde9#,
      16#bef9a3f7b2c67915#, 16#c67178f2e372532b#,
      16#ca273eceea26619c#, 16#d186b8c721c0c207#,
      16#eada7dd6cde0eb1e#, 16#f57d4f7fee6ed178#,
      16#06f067aa72176fba#, 16#0a637dc5a2c898a6#,
      16#113f9804bef90dae#, 16#1b710b35131c471b#,
      16#28db77f523047d84#, 16#32caab7b40c72493#,
      16#3c9ebe0a15c9bebc#, 16#431d67c49c100d4c#,
      16#4cc5d4becb3e42b6#, 16#597f299cfc657e2a#,
      16#5fcb6fab3ad6faec#, 16#6c44198c4a475817#
     );

   -------------------------------------------------------------------------

   procedure Padding512(Message_Block  : in out W_Block512;
                        Message_Length : in Message_Length64;
                        MP : out W_Block512) is
      A : Natural; -- which Block
      T : Word :=0;
      L : constant Word := Word(Message_Length) and 511;
   begin
      MP :=(others=>0);

      --Append the "1"-Bit
      A := Natural(Shift_Right(L, 5));
      Message_Block(A):=
        Message_Block(A) or  Shift_Left(1,Natural(31-(L and 31)));

      -- compute K
      for K in Message_Length64'Range loop
         if (Word(Message_Length + 1 + K) and 511)  = 448 then
            T:= Word(K) + L + 64;
            exit;
         end if;
      end loop;

      if T < 512 then
         -- The Message Block_size is ok.
         Message_Block(W_Block512'Last):=  Word(Message_Length);
         Message_Block(W_Block512'Last-1):=
           Word(Shift_Right(Message_Length,32));

         -- The Message Block_size is too short
         -- Let's allocate another empty message block and padd the message.
      else
         MP(W_Block512'Last)   := Word(Message_Length);
         MP(W_Block512'Last-1) := Word(Shift_Right(Message_Length,32));
      end if;
   end Padding512;

   ---------------------------------------------------------------------------

    function  Ch(X, Y, Z : DWord ) return DWord is
    begin
       return ((X and Y) xor ((not X) and Z));
   end Ch; pragma Inline (Ch);

   ---------------------------------------------------------------------------

   function Maj(X, Y, Z : DWord) return DWord is
   begin
      return ((X and Y) xor (X and Z) xor (Y and Z));
   end Maj; pragma Inline (Maj);

   ---------------------------------------------------------------------------

   function Sum0(X : DWord) return DWord is
   begin
      return (Rotate_Right(X,28) xor
              Rotate_Right(X,34) xor
              Rotate_Right(X,39));
   end Sum0;

   ---------------------------------------------------------------------------

   function Sum1(X : DWord) return DWord is
   begin
      return (Rotate_Right(X,14) xor
              Rotate_Right(X,18) xor
              Rotate_Right(X,41));
   end Sum1; pragma Inline (Sum1);

   ---------------------------------------------------------------------------

   function S0(X : DWord) return DWord is
   begin
      return (Rotate_Right(X,1) xor Rotate_Right(x,8) xor Shift_Right(X,7));
   end S0; pragma Inline (S0);

   ---------------------------------------------------------------------------

   function S1(X : DWord) return DWord is
   begin
      return (Rotate_Right(x,19) xor Rotate_Right(x,61) xor Shift_Right(X,6));
   end S1;  pragma Inline (S1);

   ---------------------------------------------------------------------------

   procedure Round_SHA2(Message_Block : in DW_Block1024;
                        Hash_Value : in out DW_Block512) is
      A, B, C, D, E, F, G, H : DWord; -- Working variables;
      W : DWords(0..79);
   begin

      -- compute and check the current message-length
      Current_Message_Length.M1 := Current_Message_Length.M1 + 1024;
      if Current_Message_Length.M1 = 0  then
         Current_Message_Length.M2 := Current_Message_Length.M2 + 1;
         if Current_Message_Length.M2 = 0  then
            raise SHA2_Constraint_Error;
         end if;
      end if;


    -- Prepare the message schedule
      W(Message_Block'Range) := DWords(Message_Block);
      for T in 16..79 loop
         W(T) := S1(W(T-2)) + W(T-7) + S0(W(T-15)) + W(T-16);
      end loop;

   -- Initialize the eight working variables
      A := Hash_Value(0);
      B := Hash_Value(1);
      C := Hash_Value(2);
      D := Hash_Value(3);
      E := Hash_Value(4);
      F := Hash_Value(5);
      G := Hash_Value(6);
      H := Hash_Value(7);

      -- lets go
      for T in W'Range loop
         declare
            t1, T2 : DWord;
         begin
            T1 := H + Sum1(E) + Ch(E,F,G) + K(T) + W(T);
            T2 := Sum0(A) + Maj(A,B,C);
            H  := G ;
            G  := F;
            F  := E;
            E  := D + T1;
            D  := C;
            C  := B;
            B  := A;
            A  := T1 + T2;
         end;
      end loop;

      -- Compute the ith intermediate hash value Hash_Value^{(i)}
      Hash_Value(0) := Hash_Value(0) + A;
      Hash_Value(1) := Hash_Value(1) + B;
      Hash_Value(2) := Hash_Value(2) + C;
      Hash_Value(3) := Hash_Value(3) + D;
      Hash_Value(4) := Hash_Value(4) + E;
      Hash_Value(5) := Hash_Value(5) + F;
      Hash_Value(6) := Hash_Value(6) + G;
      Hash_Value(7) := Hash_Value(7) + H;

   end Round_SHA2;

    ---------------------------------------------------------------------------

   procedure Padding1024(Message_Block : in out DW_Block1024;
                         MP : out DW_Block1024) is
      A : Natural; -- which Block
      T : Word := 0;
      L : constant Word := Word(Current_Message_Length.M1  and 1023);
   begin

      MP := (others => 0);

      --Append the "1"-Bit
      A := Natural(Shift_Right(L,6));
      Message_Block(A):= Message_Block(A) xor
        Shift_Left(1,63-(Natural(L and 63)));

      -- compute K
      for K in DWord'Range loop
         if ((Current_Message_Length.M1 + 1 + K) and 1023) = 896 then
            T:= Word(K) + L + 128;
            exit;
         end if;
      end loop;


       if T < 1024 then
          Message_Block(DW_Block1024'Last)   := Current_Message_Length.M1;
          Message_Block(DW_Block1024'Last-1) := Current_Message_Length.M2;
       else
          MP(DW_Block1024'Last):=  Current_Message_Length.M1;
         MP(DW_Block1024'Last-1):= Current_Message_Length.M2;
       end if;

   end Padding1024;

   ---------------------------------------------------------------------------

   function Final_Round_SHA2(Message_Block        : DW_Block1024;
                             Message_Block_Length : Message_Block_Length1024;
                             Hash_Value           : DW_Block512)
                            return DW_Block512 is
      MP : DW_Block1024;
      Mf : DW_Block1024 := Message_Block;  -- Final message block
      H  : DW_Block512  := Hash_Value;
   begin

      if  Message_Block_Length = Message_Block_Length1024'Last then
         Round_SHA2(MF, H);
         MF := (others => 0);
      else
         -- Current_Message_Length.M1 + Last_Message_Length /= 0 because
         -- Last_Message_Length < 1024
         Current_Message_Length.M1 := Current_Message_Length.M1 +
           Shift_Left(DWord(Message_Block_Length),3);
      end if;

      Padding1024(MF, MP);
      Round_SHA2(MF, H);

      if Is_Zero( DWords(MP) ) = False then
         Round_SHA2(MP, H);
      end if;

      return H;

   end Final_Round_SHA2;

   ---------------------------------------------------------------------------

   procedure Init_SHA2 is
   begin
      Current_Message_Length.M1:=0;
      Current_Message_Length.M2:=0;
   end Init_SHA2;

   ---------------------------------------------------------------------------

   procedure Padding512(This	       : in out Sha_Utils_Context;
                        Message_Block  : in out W_Block512;
                        Message_Length : in Message_Length64;
                        MP : out W_Block512) is
   begin
      Current_Message_Length := This.Current_Message_Length;
      Padding512(Message_Block  => Message_Block,
                 Message_Length => Message_Length,
                 MP             => MP);
      This.Current_Message_Length := Current_Message_Length;
   end;

   ---------------------------------------------------------------------------

   procedure Padding1024(This	       : in out Sha_Utils_Context;
                         Message_Block : in out DW_Block1024;
                         MP            : out DW_Block1024) is
   begin
      Current_Message_Length := This.Current_Message_Length;
      Padding1024(Message_Block => Message_Block,
                  MP            => MP);
      This.Current_Message_Length := Current_Message_Length;
   end;

   ---------------------------------------------------------------------------

   procedure Round_SHA2(This	      : in out Sha_Utils_Context;
                        Message_Block : in DW_Block1024;
                        Hash_Value    : in out DW_Block512) is
   begin
      Current_Message_Length := This.Current_Message_Length;
      Round_SHA2(Message_Block => Message_Block,
                 Hash_Value    => Hash_Value);
      This.Current_Message_Length := Current_Message_Length;
   end;

   ---------------------------------------------------------------------------

      function Final_Round_SHA2(This	      	  : in out Sha_Utils_Context;
                             Message_Block        : in DW_Block1024;
                             Message_Block_Length : in Message_Block_Length1024;
                             Hash_Value           : in DW_Block512)
                                return DW_Block512 is
      Return_Block : DW_Block512;
   begin
      Current_Message_Length := This.Current_Message_Length;
      Return_Block := Final_Round_SHA2(Message_Block        => Message_Block,
                                       Message_Block_Length => Message_Block_Length,
                                       Hash_Value           => Hash_Value);
      This.Current_Message_Length := Current_Message_Length;
      return Return_Block;

   end;

   ---------------------------------------------------------------------------

   procedure Init_SHA2(This : in out Sha_Utils_Context) is
   begin
      Current_Message_Length := This.Current_Message_Length;
      Init_SHA2;
      This.Current_Message_Length := Current_Message_Length;
   end;




   end  Crypto.Symmetric.Algorithm.SHA_Utils;
