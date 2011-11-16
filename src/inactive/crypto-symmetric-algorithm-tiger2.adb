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

with Crypto.Hashfunction.Tiger2.Tables;
use  Crypto.Hashfunction.Tiger2.Tables;
with Ada.Text_IO; use Ada.Text_IO;

--  Hash_Value = a||b||c
-- T = aa||bb|cc
-- Message_Block  = x

package body Crypto.Hashfunction.Tiger2 is

   package DIO is new Ada.Text_Io.Modular_IO (DWord);

   Current_Message_Length : Message_Length64;
    T : DW_Block192;

    --------------------------------------------------------------------------

   procedure Save_Hash(Hash_Value : in DW_Block192)  is
   begin
      T(0) :=   Hash_Value(0);
      T(1) :=   Hash_Value(1);
      T(2) :=   Hash_Value(2);
   end Save_Hash; pragma Inline(Save_Hash);

   --------------------------------------------------------------------------

   procedure Round(Hash_Value : in out DW_Block192; X : in DWord;
                   Mul : in DWord) is
      C : Byte_DWord :=  To_Bytes(Hash_Value(2));
   begin
      Hash_Value(2) := Hash_Value(2) xor X;
      Hash_Value(0) := Hash_Value(0) -  (T1(C(0)) xor T2(C(2)) xor
                                         T3(C(4)) xor T4(C(6)));
      Hash_Value(1) :=  Hash_Value(1) + (T4(C(1)) xor T3(C(3)) xor
                                         T2(C(5)) xor T1(C(7)));
      Hash_Value(1) :=  Hash_Value(1) * Mul;
   end Round; pragma Inline(Round);

   --------------------------------------------------------------------------

   procedure Pass(Hash_Value :  in out DW_Block192;
                  Message_Block : in DW_Block512;
                  Mul : in DWord) is
   begin
      for I in Message_Block'Range loop
         round(Hash_Value,Message_Block(I), Mul);
      end loop;
   end Pass; pragma Inline(Pass);

   --------------------------------------------------------------------------

   procedure  Key_Schedule(X : in out DW_Block512) is
   begin
      X(0) := X(0) - (X(7) xor 16#A5A5A5A5A5A5A5A5#);

      X(1) := X(1)  xor X(0);
      X(2) := X(2) + X(1);
      X(3) := X(3)  - (X(2) xor Shift_Left(not(X(1)),19));

      X(4) := X(4) xor X(3);
      X(5) := X(5) + X(4);
      X(6) := X(6) - (X(5) xor Shift_Right(not(X(4)),23));

      X(7) := X(7)  xor X(6);
      X(0) := X(0)  + X(7);
      X(1) := X(1)  - (X(0) xor Shift_Left(not(X(7)),19));

      X(2) := X(2)  xor X(1);
      X(3) := X(3)  + X(2);
      X(4) := X(4)  - (X(3) xor Shift_Right(not(X(2)),23));

      X(5) := X(5)  xor X(4);
      X(6) := X(6)  + X(5);
      X(7) := X(7)  - (X(6) xor 16#0123456789ABCDEF#);
   end Key_Schedule; pragma Inline(Key_Schedule);

   --------------------------------------------------------------------------

   procedure Feedforward(Hash_Value : in out DW_Block192) is
   begin
      Hash_Value(0) :=  Hash_Value(0) xor T(0);
      Hash_Value(1) :=  Hash_Value(1)  -  T(1);
      Hash_Value(2) :=  Hash_Value(2)  +  T(2);
   end Feedforward; pragma Inline(Feedforward);

   --------------------------------------------------------------------------


   procedure Padding_D512(Message_Block  : in out DW_Block512;
                        Message_Length :  in Message_Length64;
                        MP : out DW_Block512) is
      A : Natural; -- which Block
      T : Word;
   begin
      MP :=(others=>0);

      --Append the "1"-Bit
      A := Integer(Message_Length mod 512) / DWord'Size;
      Message_Block(A):= Message_Block(A) or
        Shift_Left(1,(DWord'Size-1)
                   -(Integer(Message_Length mod 512) mod DWord'Size));

      -- compute K
      for K in Message_Length64'Range loop
         if ((Message_Length + 1 + K) mod 512)  = 448 then
            T:= Word(K + (Message_Length mod 512) + 64);
            exit;
         end if;
      end loop;

      if T < 512 then
         -- The Message Block_size is ok.
         Message_Block(DW_Block512'Last):= Message_Block(DW_Block512'Last) +
           DWord(Message_Length);
         Message_Block(DW_Block512'Last-1):= Message_Block(DW_Block512'Last-1)
           + DWord(Shift_Left(Message_Length,DWord'Size));

         -- The Message Block_size is too short
         -- Let's allocate another empty message block and padd the message.
      else
         MP(DW_Block512'Last):= MP(DW_Block512'Last) +  DWord(Message_Length);
         MP(DW_Block512'Last-1):= MP(DW_Block512'Last-1) +
           DWord(Shift_Left(Message_Length,DWord'Size));
      end if;

      for I in Message_Block'Range loop
         DIO.PUT(Message_Block(I), Base =>16);
         New_Line;
      end loop;

   end Padding_D512;

   ---------------------------------------------------------------------------

   procedure Init_Tiger2(Hash_Value : out DW_Block192) is
   begin
      Current_Message_Length:=0;
      Hash_Value(0) := 16#0123456789ABCDEF#;
      Hash_Value(1) := 16#FEDCBA9876543210#;
      Hash_Value(2) := 16#F096A5B4C3B2E187#;
   end Init_Tiger2; pragma Inline(Init_Tiger2);

   --------------------------------------------------------------------------

   procedure Round_Tiger2(Message_Block : in DW_Block512;
                          Hash_Value    : in out DW_Block192) is
      X : DW_Block512 := Message_Block;
   begin
       Current_Message_Length := Current_Message_Length + 512;
       if Current_Message_Length = 0  then
          raise Tiger2_Constraint_Error;
       end if;
       Save_Hash(Hash_Value);
       Pass(Hash_Value,X,5);
       Key_Schedule(X);
       Pass(Hash_Value,X,7);
       Key_Schedule(X);
       Pass(Hash_Value,X,9);
       Feedforward(Hash_Value);
    end Round_Tiger2;

    --------------------------------------------------------------------------

    function Final_Round_Tiger2(Last_Message_Block  : DW_Block512;
                                Last_Message_Length : Message_Block_Length512;
                                Hash_Value          : DW_Block192)
                            return DW_Block192 is
       H  : DW_Block192 := Hash_Value;
       MF : DW_Block512 := Last_Message_Block;
       MP : DW_Block512;
    begin
       Current_Message_Length := Current_Message_Length +
         Message_length64(Last_Message_Length)*8;

       Padding_D512(MF, Current_Message_Length, MP);

       Round_Tiger2(MF, H);

       if Is_Zero(MP) = False then
          Round_Tiger2(MP, H);
       end if;

       return H;
    end Final_Round_Tiger2;

    --------------------------------------------------------------------------

    procedure Tiger2(Message : in Bytes; Hash_Value : out DW_Block192) is
       K : constant Natural :=  Message'Length/64;
       L : constant Natural :=  Message'Length mod 64;
       LM : Natural;
       M : DW_Block512 := (others => 0);
    begin
       Init_Tiger2(Hash_Value);

      for I in 1..K loop
         M :=  To_DWords(Message(1+(I-1)*64..I*64));
         Round_Tiger2(M, Hash_Value);
      end loop;

      if L /=  0 then
         LM := L / 8;
         if L mod 8 = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) :=
           To_DWords(Message(Message'Last-(L-1)..Message'Last));
      end if;

      Hash_Value :=  Final_Round_Tiger2(M, L, Hash_Value);
    end Tiger2;

    --------------------------------------------------------------------------

    procedure Tiger2(Message : in String; Hash_Value : out DW_Block192) is
    begin
       Tiger2(To_Bytes(Message), Hash_Value);
    end; pragma Inline (Tiger2);

    --------------------------------------------------------------------------

end Crypto.Hashfunction.Tiger2;
