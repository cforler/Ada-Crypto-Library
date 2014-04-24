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

with Crypto.Symmetric.Algorithm.Whirlpool.Tables;
use Crypto.Symmetric.Algorithm.Whirlpool.Tables;
with GNAT.OS_Lib; use GNAT.OS_Lib;

-- with Ada.Text_IO; use Ada.Text_IO;

package body Crypto.Symmetric.Algorithm.Whirlpool is

   Current_Message_Length : Message_Length256;

--   package BIO is new Ada.Text_Io.Modular_IO (Byte);
--   package DIO is new Ada.Text_Io.Modular_IO (DWord);
--   use BIO;
--   use DIO;

   ---------------------------------------------------------------------------

   procedure Init(Hash_Value : out DW_Block512) is
   begin
      Current_Message_Length := (others => 0);
      Hash_Value             := (others => 0);
   end Init;

   ---------------------------------------------------------------------------

   procedure Round(Message_Block : in DW_Block512;
                             Hash_Value    : in out DW_Block512) is
      State : DW_Block512;
      K     : DW_Block512 := Hash_Value;
      L     : DW_Block512 := Hash_Value;
   begin
      Current_Message_Length(0) := Current_Message_Length(0) + 512;

      if  Current_Message_Length(0) = 0 then
         Current_Message_Length(1) := Current_Message_Length(1) + 1;
         if  Current_Message_Length(1) = 0 then
            Current_Message_Length(2) := Current_Message_Length(2) + 1;
            if  Current_Message_Length(2) = 0 then
               Current_Message_Length(3) := Current_Message_Length(3) + 1;
               if Current_Message_Length(3) = 0  then
                  raise Whirlpool_Constraint_Error;
               end if;
            end if;
         end if;
      end if;

      --  compute and apply K^0 to the cipher state:
      State(0) := Message_Block(0) xor K(0);
      State(1) := Message_Block(1) xor K(1);
      State(2) := Message_Block(2) xor K(2);
      State(3) := Message_Block(3) xor K(3);
      State(4) := Message_Block(4) xor K(4);
      State(5) := Message_Block(5) xor K(5);
      State(6) := Message_Block(6) xor K(6);
      State(7) := Message_Block(7) xor K(7);

       -- iterate over all rounds:
      for I in 1..R loop
         --  compute K^r from K^{r-1}:
         L(0) :=
           C0(Byte0(K(0))) xor  C1(Byte1(K(7))) xor C2(Byte2(K(6))) xor
           C3(Byte3(K(5))) xor  C4(Byte4(K(4))) xor C5(Byte5(K(3))) xor
           C6(Byte6(K(2))) xor  C7(Byte7(K(1))) xor Rc(I);

          L(1) :=
           C0(Byte0(K(1))) xor  C1(Byte1(K(0))) xor C2(Byte2(K(7))) xor
           C3(Byte3(K(6))) xor  C4(Byte4(K(5))) xor C5(Byte5(K(4))) xor
           C6(Byte6(K(3))) xor  C7(Byte7(K(2)));

          L(2) :=
            C0(Byte0(K(2))) xor  C1(Byte1(K(1))) xor C2(Byte2(K(0))) xor
            C3(Byte3(K(7))) xor  C4(Byte4(K(6))) xor C5(Byte5(K(5))) xor
            C6(Byte6(K(4))) xor  C7(Byte7(K(3)));

          L(3) :=
            C0(Byte0(K(3))) xor  C1(Byte1(K(2))) xor C2(Byte2(K(1))) xor
            C3(Byte3(K(0))) xor  C4(Byte4(K(7))) xor C5(Byte5(K(6))) xor
            C6(Byte6(K(5))) xor  C7(Byte7(K(4)));

          L(4) :=
            C0(Byte0(K(4))) xor  C1(Byte1(K(3))) xor C2(Byte2(K(2))) xor
            C3(Byte3(K(1))) xor  C4(Byte4(K(0))) xor C5(Byte5(K(7))) xor
            C6(Byte6(K(6))) xor  C7(Byte7(K(5)));

          L(5) :=
            C0(Byte0(K(5))) xor  C1(Byte1(K(4))) xor C2(Byte2(K(3))) xor
            C3(Byte3(K(2))) xor  C4(Byte4(K(1))) xor C5(Byte5(K(0))) xor
            C6(Byte6(K(7))) xor  C7(Byte7(K(6)));

          L(6) :=
            C0(Byte0(K(6))) xor  C1(Byte1(K(5))) xor C2(Byte2(K(4))) xor
            C3(Byte3(K(3))) xor  C4(Byte4(K(2))) xor C5(Byte5(K(1))) xor
            C6(Byte6(K(0))) xor  C7(Byte7(K(7)));

          L(7) :=
            C0(Byte0(K(7))) xor  C1(Byte1(K(6))) xor C2(Byte2(K(5))) xor
            C3(Byte3(K(4))) xor  C4(Byte4(K(3))) xor C5(Byte5(K(2))) xor
            C6(Byte6(K(1))) xor  C7(Byte7(K(0)));

          K := L;

          -- apply the r-th round transformation:
          L(0) :=
            C0(Byte0(State(0))) xor C1(Byte1(State(7))) xor
            C2(Byte2(State(6))) xor C3(Byte3(State(5))) xor
            C4(Byte4(State(4))) xor C5(Byte5(State(3))) xor
            C6(Byte6(State(2))) xor C7(Byte7(State(1))) xor
            K(0);

          L(1) :=
            C0(Byte0(State(1))) xor C1(Byte1(State(0))) xor
            C2(Byte2(State(7))) xor C3(Byte3(State(6))) xor
            C4(Byte4(State(5))) xor C5(Byte5(State(4))) xor
            C6(Byte6(State(3))) xor C7(Byte7(State(2))) xor
            K(1);

          L(2) :=
            C0(Byte0(State(2))) xor C1(Byte1(State(1))) xor
            C2(Byte2(State(0))) xor C3(Byte3(State(7))) xor
            C4(Byte4(State(6))) xor C5(Byte5(State(5))) xor
            C6(Byte6(State(4))) xor C7(Byte7(State(3))) xor
            K(2);

          L(3) :=
            C0(Byte0(State(3))) xor C1(Byte1(State(2))) xor
            C2(Byte2(State(1))) xor C3(Byte3(State(0))) xor
            C4(Byte4(State(7))) xor C5(Byte5(State(6))) xor
            C6(Byte6(State(5))) xor C7(Byte7(State(4))) xor
            K(3);

          L(4) :=
            C0(Byte0(State(4))) xor C1(Byte1(State(3))) xor
            C2(Byte2(State(2))) xor C3(Byte3(State(1))) xor
            C4(Byte4(State(0))) xor C5(Byte5(State(7))) xor
            C6(Byte6(State(6))) xor C7(Byte7(State(5))) xor
            K(4);

          L(5) :=
            C0(Byte0(State(5))) xor C1(Byte1(State(4))) xor
            C2(Byte2(State(3))) xor C3(Byte3(State(2))) xor
            C4(Byte4(State(1))) xor C5(Byte5(State(0))) xor
            C6(Byte6(State(7))) xor C7(Byte7(State(6))) xor
            K(5);

          L(6) :=
            C0(Byte0(State(6))) xor C1(Byte1(State(5))) xor
            C2(Byte2(State(4))) xor C3(Byte3(State(3))) xor
            C4(Byte4(State(2))) xor C5(Byte5(State(1))) xor
            C6(Byte6(State(0))) xor C7(Byte7(State(7))) xor
            K(6);

          L(7) :=
            C0(Byte0(State(7))) xor C1(Byte1(State(6))) xor
            C2(Byte2(State(5))) xor C3(Byte3(State(4))) xor
            C4(Byte4(State(3))) xor C5(Byte5(State(2))) xor
            C6(Byte6(State(1))) xor C7(Byte7(State(0))) xor
            K(7);

          State := L;
      end loop;

      -- apply the Miyaguchi-Preneel compression function:
      Hash_Value := DW_Block512(DWords(Hash_Value) xor DWords(State) 
				  xor DWords(Message_Block));

   end Round;

   ---------------------------------------------------------------------------


   procedure Padding(Message_Block  : in out DW_Block512;
                     MP : out DW_Block512) is
      A : Natural; -- which Block
      T : Word := 0;
      L : constant Word := Word(Current_Message_Length(0) and 511);
   begin
      MP :=(others=>0);

      --Append the "1"-Bit
      A := Natural(Shift_Right(L, 6));
      Message_Block(A):=
        Message_Block(A) or Shift_Left(1,Natural(63-(L and 63)));

      -- compute K
      for K in Word'Range loop
         if ((L + 1 + K) and 511)  = 256 then
            T:= K + L + 256;
            exit;
         end if;
      end loop;

      if T < 512 then
         -- The Message Block_size is ok.
         Message_Block(DW_Block512'Last)   := Current_Message_Length(0);
         Message_Block(DW_Block512'Last-1) := Current_Message_Length(1);
         Message_Block(DW_Block512'Last-2) := Current_Message_Length(2);
         Message_Block(DW_Block512'Last-3) := Current_Message_Length(3);

         -- The Message Block_size is too short
         -- Let's allocate another empty message block and padd the message.
      else
         MP(DW_Block512'Last)   := Current_Message_Length(0);
         MP(DW_Block512'Last-1) := Current_Message_Length(1);
         MP(DW_Block512'Last-2) := Current_Message_Length(2);
         MP(DW_Block512'Last-3) := Current_Message_Length(3);
      end if;
   end Padding;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block : DW_Block512;
                                  Last_Message_Length: Message_Block_Length512;
                                  Hash_Value         : DW_Block512)
                                 return DW_Block512 is
      MP : DW_Block512;
      Mf : DW_Block512 := Last_Message_Block;  -- Final message block
      H  : DW_Block512  := Hash_Value;
   begin
      if  Last_Message_Length = Message_Block_Length512'Last then
         Round(MF, H);
         MF := (others => 0);
      else
         -- Current_Message_Length(0) + Last_Message_Length /= 0 because
         -- Last_Message_Length < 512
         Current_Message_Length(0) := Current_Message_Length(0) +
           Shift_Left(DWord(Last_Message_Length),3);
      end if;

      Padding(MF, MP);
      Round(MF, H);

      if Is_Zero( DWords(MP) ) = False then
         Round(MP, H);
      end if;

      return H;

   end Final_Round;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in Bytes; Hash_Value : out DW_Block512) is
      -- K == |Message| mod 512 == number of full message blocks
      K : constant Natural :=  Message'Length/64;
      -- L = length of the last message
      L : constant Natural :=  Message'Length mod 64;
      LM : Natural := Message'First;
      M : DWords(DW_Block512'Range) := (others=>0);
   begin
      Init(Hash_Value);

      for I in 1..K loop
	 declare
	    T : constant DWords :=  To_DWords(Message(LM..LM+63));
	 begin
	    Round(DW_Block512(T), Hash_Value);
	 end;
	 LM := LM+64;
      end loop;

      if L /=  0 then
         LM := L/8;
         if (L mod 8) = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) :=
           To_DWords(Message(Message'Last-(L-1)..Message'Last));
      end if;
      Hash_Value :=  Final_Round(DW_Block512(M), L, Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in String; Hash_Value : out DW_Block512) is
   begin
      Hash(To_Bytes(Message), Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure F_Hash(Filename : in String; Hash_Value : out DW_Block512) is
      Size     : Integer;
      Mcounter : Message_Counter_Type:=0;
      Bcounter : To_DWord_Counter_Type:=0;
      M        : DW_Block512 := (others => 0); -- Messageblock
      B        : Byte_DWord:=(others=>0);
      Buf      : Bytes(0..64);
      Fd       : File_Descriptor;
      FilePath : constant string := Filename & ASCII.NUL;

   begin
      Fd :=  Open_Read(FilePath'Address, Binary);
      if fd = invalid_fd then
         raise File_Open_Error;
      end if;

      Init(Hash_Value);

      loop
         Size := Read(Fd, Buf'Address , Buf'Last);
         if Size = Buf'Last then
	    declare
	       T : constant DWords := To_DWords(Buf(0..63));
	    begin
	       Round(DW_Block512(T),Hash_Value);
	    end;
         elsif
           Size < 0 then raise File_Read_Error;
         else
            for I in 0..Size-1 loop
               B(Integer(Bcounter)):= Buf(I);
               Bcounter:=Bcounter+1;
               if Bcounter = 0 then
                  M(Integer(Mcounter)):= To_DWord(B);
                  B:=(others=>0);
                  Mcounter:= Mcounter+1;
               end if;
            end loop;
            exit;
         end if;
      end loop;

      Close(Fd);

      if BCounter /= 0 then
         M(Integer(MCounter)):= To_DWord(B);
      end if;

      Hash_Value := Final_Round(M, Message_Block_Length512(Size), Hash_Value);

   end F_Hash;

   ---------------------------------------------------------------------------
   
   procedure Init(This 		: in out Whirlpool_Interface) is
   begin
      This.Current_Message_Length := (others => 0);
      This.Hash_Value             := (others => 0);
   end Init;
   
   
      ---------------------------------------------------------------------------
   
   procedure Padding(This	    : in out Whirlpool_Interface;
                     Message_Block  : in out DW_Block512;
                     MP : out DW_Block512) is
      A : Natural; -- which Block
      T : Word := 0;
      L : constant Word := Word(This.Current_Message_Length(0) and 511);
   begin
      MP :=(others=>0);

      --Append the "1"-Bit
      A := Natural(Shift_Right(L, 6));
      Message_Block(A):=
        Message_Block(A) or Shift_Left(1,Natural(63-(L and 63)));

      -- compute K
      for K in Word'Range loop
         if ((L + 1 + K) and 511)  = 256 then
            T:= K + L + 256;
            exit;
         end if;
      end loop;

      if T < 512 then
         -- The Message Block_size is ok.
         Message_Block(DW_Block512'Last)   := This.Current_Message_Length(0);
         Message_Block(DW_Block512'Last-1) := This.Current_Message_Length(1);
         Message_Block(DW_Block512'Last-2) := This.Current_Message_Length(2);
         Message_Block(DW_Block512'Last-3) := This.Current_Message_Length(3);

         -- The Message Block_size is too short
         -- Let's allocate another empty message block and padd the message.
      else
         MP(DW_Block512'Last)   := This.Current_Message_Length(0);
         MP(DW_Block512'Last-1) := This.Current_Message_Length(1);
         MP(DW_Block512'Last-2) := This.Current_Message_Length(2);
         MP(DW_Block512'Last-3) := This.Current_Message_Length(3);
      end if;
   end Padding;
   ---------------------------------------------------------------------------
   
   procedure Round(This 	: in out 	Whirlpool_Interface;
                   Message_Block: in 		DW_Block512) is
      State : DW_Block512;
      K     : DW_Block512 := This.Hash_Value;
      L     : DW_Block512 := This.Hash_Value;
   begin
      This.Current_Message_Length(0) := This.Current_Message_Length(0) + 512;

      if  This.Current_Message_Length(0) = 0 then
         This.Current_Message_Length(1) := This.Current_Message_Length(1) + 1;
         if  This.Current_Message_Length(1) = 0 then
            This.Current_Message_Length(2) := This.Current_Message_Length(2) + 1;
            if  This.Current_Message_Length(2) = 0 then
               This.Current_Message_Length(3) := This.Current_Message_Length(3) + 1;
               if This.Current_Message_Length(3) = 0  then
                  raise Whirlpool_Constraint_Error;
               end if;
            end if;
         end if;
      end if;

      --  compute and apply K^0 to the cipher state:
      State(0) := Message_Block(0) xor K(0);
      State(1) := Message_Block(1) xor K(1);
      State(2) := Message_Block(2) xor K(2);
      State(3) := Message_Block(3) xor K(3);
      State(4) := Message_Block(4) xor K(4);
      State(5) := Message_Block(5) xor K(5);
      State(6) := Message_Block(6) xor K(6);
      State(7) := Message_Block(7) xor K(7);

       -- iterate over all rounds:
      for I in 1..R loop
         --  compute K^r from K^{r-1}:
         L(0) :=
           C0(Byte0(K(0))) xor  C1(Byte1(K(7))) xor C2(Byte2(K(6))) xor
           C3(Byte3(K(5))) xor  C4(Byte4(K(4))) xor C5(Byte5(K(3))) xor
           C6(Byte6(K(2))) xor  C7(Byte7(K(1))) xor Rc(I);

          L(1) :=
           C0(Byte0(K(1))) xor  C1(Byte1(K(0))) xor C2(Byte2(K(7))) xor
           C3(Byte3(K(6))) xor  C4(Byte4(K(5))) xor C5(Byte5(K(4))) xor
           C6(Byte6(K(3))) xor  C7(Byte7(K(2)));

          L(2) :=
            C0(Byte0(K(2))) xor  C1(Byte1(K(1))) xor C2(Byte2(K(0))) xor
            C3(Byte3(K(7))) xor  C4(Byte4(K(6))) xor C5(Byte5(K(5))) xor
            C6(Byte6(K(4))) xor  C7(Byte7(K(3)));

          L(3) :=
            C0(Byte0(K(3))) xor  C1(Byte1(K(2))) xor C2(Byte2(K(1))) xor
            C3(Byte3(K(0))) xor  C4(Byte4(K(7))) xor C5(Byte5(K(6))) xor
            C6(Byte6(K(5))) xor  C7(Byte7(K(4)));

          L(4) :=
            C0(Byte0(K(4))) xor  C1(Byte1(K(3))) xor C2(Byte2(K(2))) xor
            C3(Byte3(K(1))) xor  C4(Byte4(K(0))) xor C5(Byte5(K(7))) xor
            C6(Byte6(K(6))) xor  C7(Byte7(K(5)));

          L(5) :=
            C0(Byte0(K(5))) xor  C1(Byte1(K(4))) xor C2(Byte2(K(3))) xor
            C3(Byte3(K(2))) xor  C4(Byte4(K(1))) xor C5(Byte5(K(0))) xor
            C6(Byte6(K(7))) xor  C7(Byte7(K(6)));

          L(6) :=
            C0(Byte0(K(6))) xor  C1(Byte1(K(5))) xor C2(Byte2(K(4))) xor
            C3(Byte3(K(3))) xor  C4(Byte4(K(2))) xor C5(Byte5(K(1))) xor
            C6(Byte6(K(0))) xor  C7(Byte7(K(7)));

          L(7) :=
            C0(Byte0(K(7))) xor  C1(Byte1(K(6))) xor C2(Byte2(K(5))) xor
            C3(Byte3(K(4))) xor  C4(Byte4(K(3))) xor C5(Byte5(K(2))) xor
            C6(Byte6(K(1))) xor  C7(Byte7(K(0)));

          K := L;

          -- apply the r-th round transformation:
          L(0) :=
            C0(Byte0(State(0))) xor C1(Byte1(State(7))) xor
            C2(Byte2(State(6))) xor C3(Byte3(State(5))) xor
            C4(Byte4(State(4))) xor C5(Byte5(State(3))) xor
            C6(Byte6(State(2))) xor C7(Byte7(State(1))) xor
            K(0);

          L(1) :=
            C0(Byte0(State(1))) xor C1(Byte1(State(0))) xor
            C2(Byte2(State(7))) xor C3(Byte3(State(6))) xor
            C4(Byte4(State(5))) xor C5(Byte5(State(4))) xor
            C6(Byte6(State(3))) xor C7(Byte7(State(2))) xor
            K(1);

          L(2) :=
            C0(Byte0(State(2))) xor C1(Byte1(State(1))) xor
            C2(Byte2(State(0))) xor C3(Byte3(State(7))) xor
            C4(Byte4(State(6))) xor C5(Byte5(State(5))) xor
            C6(Byte6(State(4))) xor C7(Byte7(State(3))) xor
            K(2);

          L(3) :=
            C0(Byte0(State(3))) xor C1(Byte1(State(2))) xor
            C2(Byte2(State(1))) xor C3(Byte3(State(0))) xor
            C4(Byte4(State(7))) xor C5(Byte5(State(6))) xor
            C6(Byte6(State(5))) xor C7(Byte7(State(4))) xor
            K(3);

          L(4) :=
            C0(Byte0(State(4))) xor C1(Byte1(State(3))) xor
            C2(Byte2(State(2))) xor C3(Byte3(State(1))) xor
            C4(Byte4(State(0))) xor C5(Byte5(State(7))) xor
            C6(Byte6(State(6))) xor C7(Byte7(State(5))) xor
            K(4);

          L(5) :=
            C0(Byte0(State(5))) xor C1(Byte1(State(4))) xor
            C2(Byte2(State(3))) xor C3(Byte3(State(2))) xor
            C4(Byte4(State(1))) xor C5(Byte5(State(0))) xor
            C6(Byte6(State(7))) xor C7(Byte7(State(6))) xor
            K(5);

          L(6) :=
            C0(Byte0(State(6))) xor C1(Byte1(State(5))) xor
            C2(Byte2(State(4))) xor C3(Byte3(State(3))) xor
            C4(Byte4(State(2))) xor C5(Byte5(State(1))) xor
            C6(Byte6(State(0))) xor C7(Byte7(State(7))) xor
            K(6);

          L(7) :=
            C0(Byte0(State(7))) xor C1(Byte1(State(6))) xor
            C2(Byte2(State(5))) xor C3(Byte3(State(4))) xor
            C4(Byte4(State(3))) xor C5(Byte5(State(2))) xor
            C6(Byte6(State(1))) xor C7(Byte7(State(0))) xor
            K(7);

          State := L;
      end loop;

      -- apply the Miyaguchi-Preneel compression function:
      This.Hash_Value := DW_Block512(DWords(This.Hash_Value) xor DWords(State) 
				  xor DWords(Message_Block));

   end Round;
   
   function Final_Round(This 		    : in out Whirlpool_Interface;
                        Last_Message_Block  : DW_Block512;
                        Last_Message_Length : Message_Block_Length512)
                        return DW_Block512 is
      MP : DW_Block512;
      Mf : DW_Block512 := Last_Message_Block;  -- Final message block
      H  : DW_Block512  := This.Hash_Value;
   begin
      if  Last_Message_Length = Message_Block_Length512'Last then
         This.Round(Message_Block => MF);
         MF := (others => 0);
      else
         -- Current_Message_Length(0) + Last_Message_Length /= 0 because
         -- Last_Message_Length < 512
         This.Current_Message_Length(0) := This.Current_Message_Length(0) +
           Shift_Left(DWord(Last_Message_Length),3);
      end if;

      Padding(This	    => This,
              Message_Block => MF,
              MP            => MP);
      This.Round(Message_Block => MF);

      if Is_Zero( DWords(MP) ) = False then
         This.Round(Message_Block => MF);
      end if;

      return H;

   end Final_Round;



   
      

end Crypto.Symmetric.Algorithm.Whirlpool;
