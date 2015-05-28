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

-- This implementation of SHA-256 based on the FIPS 180-2
-- For further Information read FIPS 180-2
-- BTW. This paper is really cool.


with Crypto.Symmetric.Algorithm.SHA_Utils;
use  Crypto.Symmetric.Algorithm.SHA_Utils;
with  Ada.Streams.Stream_IO;

package body Crypto.Symmetric.Algorithm.SHA256 is

   Current_Message_Length : Message_Length64;

    K :  constant Words(0..63) :=
      (
       16#428a2f98#, 16#71374491#, 16#B5c0fbcf#, 16#E9b5dba5#,
       16#3956c25b#, 16#59f111f1#, 16#923f82a4#, 16#Ab1c5ed5#,
       16#D807aa98#, 16#12835b01#, 16#243185be#, 16#550c7dc3#,
       16#72be5d74#, 16#80deb1fe#, 16#9bdc06a7#, 16#C19bf174#,
       16#E49b69c1#, 16#Efbe4786#, 16#0fc19dc6#, 16#240ca1cc#,
       16#2de92c6f#, 16#4a7484aa#, 16#5cb0a9dc#, 16#76f988da#,
       16#983e5152#, 16#A831c66d#, 16#B00327c8#, 16#Bf597fc7#,
       16#C6e00bf3#, 16#D5a79147#, 16#06ca6351#, 16#14292967#,
       16#27b70a85#, 16#2e1b2138#, 16#4d2c6dfc#, 16#53380d13#,
       16#650a7354#, 16#766a0abb#, 16#81c2c92e#, 16#92722c85#,
       16#A2bfe8a1#, 16#A81a664b#, 16#C24b8b70#, 16#C76c51a3#,
       16#D192e819#, 16#D6990624#, 16#F40e3585#, 16#106aa070#,
       16#19a4c116#, 16#1e376c08#, 16#2748774c#, 16#34b0bcb5#,
       16#391c0cb3#, 16#4ed8aa4a#, 16#5b9cca4f#, 16#682e6ff3#,
       16#748f82ee#, 16#78a5636f#, 16#84c87814#, 16#8cc70208#,
       16#90befffa#, 16#A4506ceb#, 16#Bef9a3f7#, 16#C67178f2#
      );

    ---------------------------------------------------------------------------

    function  Ch(X, Y, Z : Word ) return Word is
    begin
       return ((X and Y) xor ((not X) and Z));
   end Ch; pragma Inline (Ch);

   ---------------------------------------------------------------------------

   function Maj(X,Y,Z : Word) return Word is
   begin
      return ((X and Y) xor (X and Z) xor (Y and Z));
   end Maj; pragma Inline (Maj);

   ---------------------------------------------------------------------------

   function Sum0(X : Word) return Word is
   begin
      return (Rotate_Right(X,2) xor Rotate_Right(X,13) xor Rotate_Right(X,22));
   end Sum0; pragma Inline(Sum0);

   ---------------------------------------------------------------------------

   function Sum1(X : Word) return Word is
   begin
      return (Rotate_Right(X,6) xor Rotate_Right(X,11) xor Rotate_Right(X,25));
   end Sum1; pragma Inline (Sum1);

   ---------------------------------------------------------------------------

   function S0(X : Word) return Word is
   begin
      return (Rotate_Right(X,7) xor Rotate_Right(X,18) xor Shift_Right(X,3));
   end S0; pragma Inline (S0);

   ---------------------------------------------------------------------------

   function S1(X : Word) return Word is
   begin
      return (Rotate_Right(X,17) xor Rotate_Right(X,19) xor Shift_Right(X,10));
   end S1;  pragma Inline (S1);

   ---------------------------------------------------------------------------

   procedure Init(Hash_Value : out W_Block256) is
   begin
      Current_Message_Length:=0;
      Hash_Value(0) := 16#6a09e667#;
      Hash_Value(1) := 16#Bb67ae85#;
      Hash_Value(2) := 16#3c6ef372#;
      Hash_Value(3) := 16#A54ff53a#;
      Hash_Value(4) := 16#510e527f#;
      Hash_Value(5) := 16#9b05688c#;
      Hash_Value(6) := 16#1f83d9ab#;
      Hash_Value(7) := 16#5be0cd19#;
   end Init;

   ---------------------------------------------------------------------------

   procedure Round(Message_Block : in W_Block512;
                   Hash_Value    : in out W_Block256) is
      A, B, C, D, E, F, G, H : Word; -- Working variables;
      W : Words(0..63);
   begin

      Current_Message_Length := Current_Message_Length + 512;

      if Current_Message_Length = 0  then
         raise SHA256_Constraint_Error;
      end if;

      -- Prepare the message schedule
      W(Message_Block'Range) := Words(Message_Block);
      for T in 16..63 loop
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

      for T in W'Range loop
         declare
            T1, T2 : Word;
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

   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block  : W_Block512;
                        Last_Message_Length : Message_Block_Length512;
                        Hash_Value          : W_Block256)
                       return W_Block256 is
      MP : W_Block512;
      MF : W_Block512 := Last_Message_Block;
      H  : W_Block256 := Hash_Value;
   begin
      -- If the last_message Block is a full block
      if Last_Message_Length = Message_Block_Length512'Last then
         Round(MF, H);
         MF := (others => 0);
      else
         Current_Message_Length := Current_Message_Length +
           Message_Length64(Last_Message_Length)*8;
      end if;

      Padding512(MF, Current_Message_Length, MP);
      Round(MF, H);

      if Is_Zero( Words(MP) ) = False then
         Round(MP, H);
      end if;
      return H;
   end Final_Round;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in Bytes; Hash_Value : out W_Block256) is
      K : constant Natural :=  Message'Length/64;
      L : constant Natural :=  Message'Length mod 64;
      LM : Natural := Message'First;
      M : Words(W_Block512'Range) := (others=>0);

   begin
      Init(Hash_Value);

      for I in 1..K loop
         Round(W_Block512(To_Words(Message(LM..LM+63))),Hash_Value);
	 LM := LM+64;
      end loop;

      if L /=  0 then
         LM := L/4;
         if L mod 4 = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) := To_Words(Message(Message'Last-(L-1)..Message'Last));
      end if;
      Hash_Value := Final_Round(W_Block512(M), L, Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in String; Hash_Value : out W_Block256) is
   begin
      Hash(To_Bytes(Message), Hash_Value);
   end;


   ---------------------------------------------------------------------------

   procedure F_Hash(Filename : in String; Hash_Value : out W_Block256) is
      use Ada.Streams.Stream_IO;

      Buf      : Bytes(1..64);
      Fd       : File_Type;
      Fmode    : constant File_Mode := In_File;
      Remaining_Bytes : Natural := 0;
   begin
      Open(File => Fd,
           Mode => Fmode,
           Name => Filename);

      if not Is_Open(Fd) then
         raise File_Open_Error;
      end if;

      Init(Hash_Value);

      while not End_Of_File(Fd) loop
         Remaining_Bytes := Natural(Size(Fd) - (Index(Fd)-1));
         if (Remaining_Bytes > Buf'Last ) then
           Bytes'Read(Stream(Fd), Buf);
           Round( W_Block512(To_Words(Buf(1..64)) ),Hash_Value);
         else
            declare
               Last_Bytes : Bytes(1..Integer(Remaining_Bytes));
            begin
               Bytes'Read(Stream(Fd), Last_Bytes);
               --last block
               Buf := (others => 0);
               Buf(1..Last_Bytes'Last) := Last_Bytes;
               Hash_Value := Final_Round( W_Block512(To_Words(Buf(1..64)) ),
                                          Message_Block_Length512(Remaining_Bytes),
                                          Hash_Value);
            end;
         end if;
      end loop;

      Close(Fd);
   end F_Hash;

   ---------------------------------------------------------------------------

   procedure Init(This 		: in out SHA256_Context) is
   begin
      This.Current_Message_Length:=0;
      This.Hash_Value(0) := 16#6a09e667#;
      This.Hash_Value(1) := 16#Bb67ae85#;
      This.Hash_Value(2) := 16#3c6ef372#;
      This.Hash_Value(3) := 16#A54ff53a#;
      This.Hash_Value(4) := 16#510e527f#;
      This.Hash_Value(5) := 16#9b05688c#;
      This.Hash_Value(6) := 16#1f83d9ab#;
      This.Hash_Value(7) := 16#5be0cd19#;
   end Init;


   procedure Round(This 	: in out 	SHA256_Context;
                   Message_Block: in 		W_Block512) is
      A, B, C, D, E, F, G, H : Word; -- Working variables;
      W : Words(0..63);
   begin

      This.Current_Message_Length := This.Current_Message_Length + 512;

      if This.Current_Message_Length = 0  then
         raise SHA256_Constraint_Error;
      end if;

      -- Prepare the message schedule
      W(Message_Block'Range) := Words(Message_Block);
      for T in 16..63 loop
         W(T) := S1(W(T-2)) + W(T-7) + S0(W(T-15)) + W(T-16);
      end loop;

      -- Initialize the eight working variables
      A := This.Hash_Value(0);
      B := This.Hash_Value(1);
      C := This.Hash_Value(2);
      D := This.Hash_Value(3);
      E := This.Hash_Value(4);
      F := This.Hash_Value(5);
      G := This.Hash_Value(6);
      H := This.Hash_Value(7);

      for T in W'Range loop
         declare
            T1, T2 : Word;
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
      This.Hash_Value(0) := This.Hash_Value(0) + A;
      This.Hash_Value(1) := This.Hash_Value(1) + B;
      This.Hash_Value(2) := This.Hash_Value(2) + C;
      This.Hash_Value(3) := This.Hash_Value(3) + D;
      This.Hash_Value(4) := This.Hash_Value(4) + E;
      This.Hash_Value(5) := This.Hash_Value(5) + F;
      This.Hash_Value(6) := This.Hash_Value(6) + G;
      This.Hash_Value(7) := This.Hash_Value(7) + H;

   end Round;

   function Final_Round(This 		    : in out SHA256_Context;
                        Last_Message_Block  : W_Block512;
                        Last_Message_Length : Message_Block_Length512)
                        return W_Block256 is

      MP : W_Block512;
      MF : W_Block512 := Last_Message_Block;
      H  : W_Block256 := This.Hash_Value;
   begin
      -- If the last_message Block is a full block
      if Last_Message_Length = Message_Block_Length512'Last then
         Round(MF, H);
         MF := (others => 0);
      else
         This.Current_Message_Length := This.Current_Message_Length +
           Message_Length64(Last_Message_Length)*8;
      end if;

      Padding512(MF, This.Current_Message_Length, MP);
      Round(MF, H);

      if Is_Zero( Words(MP) ) = False then
         Round(MP, H);
      end if;
      return H;
   end Final_Round;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.SHA256;
