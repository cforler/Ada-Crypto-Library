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

-- Original Source code author: Martin Kausche, 2008.
-- This source code is released to the public domain.

--with skein_debug;               use skein_debug;
with Ada.Text_IO;			use Ada.Text_IO;


package body Crypto.Symmetric.Algorithm.Skein is

   package threefish renames Crypto.Symmetric.Algorithm.Threefish;

   -- low level API with object
   procedure Init(This 		: in out Skein_512_Context) is
      State : Bytes(0..64);
      Mode : constant threefish.Skein_Mode := m512;
   begin
      Init(Mode  => Mode,
           N_0   => 512,
           State => State);
      This.Hash_Value :=To_W_Block512(State);
   end Init;


   procedure Round(This 	: in out 	Skein_512_Context;
                   Message_Block: in 		W_Block512) is
      State : Bytes(0..64);
      Mode : constant threefish.Skein_Mode := m512;
   begin
      Update(Mode           => Mode,
             Old_State      => To_Bytes(This.Hash_Value),
             Message        => To_Bytes(Message_Block),
             Message_Length => 512,
             New_State      => State);
      This.Hash_Value := To_W_Block512(State);
   end Round;


   function Final_Round(This 		    : in out Skein_512_Context;
                        Last_Message_Block  : W_Block512;
                        Last_Message_Length : Natural)
                        return W_Block512 is
      State : Bytes(0..64);
      Mode : constant threefish.Skein_Mode := m512;
   begin
      Update(Mode           => Mode,
             Old_State      => To_Bytes(This.Hash_Value),
             Message        => To_Bytes(Last_Message_Block),
             Message_Length => Last_Message_Length*8,
             New_State      => State);
      This.Hash_Value := To_W_Block512(State);
      Final(Mode      => Mode,
            Old_State => To_Bytes(This.Hash_Value),
            N_0       => Last_Message_Length,
            New_State => State);
      return To_W_Block512(State);
   end Final_Round;

   -- high level API

   procedure Hash(Message : in String; Hash_Value : out W_Block512) is
      State : Bytes(0..64);
      Mode : constant threefish.Skein_Mode := m512;
   begin
      Skein_Complete(Mode           => Mode,
           Output_Length_Bits            => 512,
           Message        => To_Bytes(Message),
           Message_Length_Bits => Message'Length*8,
           Result         => State);
      Hash_Value := To_W_Block512(State);
   end Hash;


   procedure Hash(Message : in Bytes;  Hash_Value : out W_Block512)is
      State : Bytes(0..64);
      Mode : constant threefish.Skein_Mode := m512;
   begin

      Skein_Complete(Mode           => Mode,
           Output_Length_Bits            => 512,
           Message        => Message,
           Message_Length_Bits => Message'Length*8,
           Result         => State);
      Hash_Value := To_W_Block512(State);
   end Hash;


   --correct implementation missing
   procedure F_Hash(Filename : in String; Hash_Value : out W_Block512) is
   begin
      null;
   end F_Hash;





   procedure Set_Bit
     (b        : in out Byte;
      Position : in Natural;
      Value    : in Boolean)
   is
   begin
      if Value then
         b := b or Byte (2 ** Position);
      else
         b := b and Byte'Last - Byte (2 ** Position);
      end if;
   end Set_Bit;

   function Create
     (Size : Natural)
      return Skein_Message_Tweak_Tuple_Pointer_Array
   is
      foo : Skein_Message_Tweak_Tuple_Pointer_Array (0 .. Size - 1);
   begin
      return foo;
   end Create;

   procedure Set_Data
     (List                : in out Skein_Message_Tweak_Tuple_Pointer_Array;
      Index               : in Natural;
      Message             : in Bytes;
      Message_Length_Bits : in Natural;
      Type_Value          : in Byte)
   is
   begin
      List (Index)                    :=
        new Skein_Message_Tweak_Tuple
        (Message_Length_Bits => Message_Length_Bits,
         Message_Bytes_Last => Message'Last);
      List (Index).all.Message        := Message;
      List (Index).all.Message_Length := Message_Length_Bits;
      List (Index).all.Type_Value     := Type_Value;
   end Set_Data;

   function Message_Bit_Padding
     (Message        : in Bytes;
      Desired_Length : in Natural)
      return           Bytes
   is
      Local_Message : Bytes := Message;
   begin
      if Desired_Length / 8 > Message'Length then
         -- the input message is to short and we would have to
         --fill more than one Byte
         Put_Line
           ("The input-Message is shorter than the given" &
            "Message-length, please check!");
         raise Program_Error;
      elsif Desired_Length / 8 = Message'Length then
         --we have to do nothing here, the message already
         --has the desired length and we will use every Bit
         return Local_Message;
      elsif Desired_Length mod 8 = 0 then
         --if we have longer inputs, but the desired length is a multiple of
         --a full Byte, then we have to do nothing but send it back
         return Local_Message (0 .. (Desired_Length / 8) - 1);
      else
         --only parts of the input message will be used
         --the rest will be padded with one true-Bit in the
         --most significant not used Bit-Position
         --the remaining Bits will be set to false
         Set_Bit
           (b        => Local_Message ((Desired_Length / 8)),  --counting
                                                               --begins at 0
            Position => 7 - (Desired_Length mod 8),
            Value    => True);
         for i in (Desired_Length mod 8) + 1 .. 7 loop
            Set_Bit
              (b        => Local_Message ((Desired_Length / 8)),  --counting
                                                                  --begins at 0
               Position => 7 - i,
               Value    => False);
         end loop;

         --only return the Bytes we really want to use
         Put_Line ("Desired_Lenght" & Integer'Image (Desired_Length));
         Put_Line
           ("We returned from 0 to" & Integer'Image (Desired_Length / 8));
         return Local_Message (0 .. Desired_Length / 8);
      end if;
   end Message_Bit_Padding;

   function Get_Bit_Padding_Status
     (Message        : in Bytes;
      Desired_Length : in Natural)
      return           Boolean
   is
   begin
      return not (Desired_Length / 8 = Message'Length);
   end Get_Bit_Padding_Status;

   function Message_Byte_Padding
     (Mode             : in Skein_Mode;
      Original_Message : in Bytes)
      return             Bytes
   is
      function Get_P (Original_Message : Bytes) return Natural is
      begin
         if Original_Message'Length = 0 then
            return Get_Number_Of_Skein_Bytes (Mode);
         elsif Original_Message'Length mod Get_Number_Of_Skein_Bytes (Mode) =
               0
         then
            return 0;
         else
            return Get_Number_Of_Skein_Bytes (Mode) -
                   (Original_Message'Length mod
                    Get_Number_Of_Skein_Bytes (Mode));
         end if;
      end Get_P;
      p              : constant Natural := Get_P (Original_Message);
      Padded_Message : Bytes (0 .. Original_Message'Length - 1 + p);
   begin
      --write the original_message to the Padded Message
      Padded_Message (0 .. Original_Message'Length - 1) := Original_Message;

      --Ada.text_IO.Put_Line("padding with p=" & Integer'Image(P));
      for i in reverse 1 .. p loop
         Padded_Message (Original_Message'Length - 1 + i) := Byte (0);
         --Ada.Text_IO.Put(Show_Hex(Padded_Message(Original_Message'Last + i))
         --& " ");
      end loop;

      return Padded_Message;
   end Message_Byte_Padding;

   function Get_Current_Tweak
     (T_S       : in Bytes;
      N_M       : in DWord;      --Number of Bytes in Input
                                                --Message
      Index     : in Natural;    --index of Message Block we are curently
                                 --working on
      N_b       : in Natural;    --Number State-Bytes for the current mode
      First_Run : in Boolean;    --a_i
      Last_Run  : in Boolean;    --b_i
      B         : in Boolean)    --was there any BitPadding?
      return      Bytes
   is
      Current_Tweak : Bytes := T_S;
   begin
      --first we just add the current already calculated bytes,
      --or if we are on the last word then we just add the number of bytes
      --at all (this only differs if the length of the input was not
      --a multiple of N_b
      --the first 96 Bits are reserved for this

      --we just use 2**64,
      --add min(N_M, (i+1)*N_b)
      --we really need to add here because in case of treehashing
      --there will already be some "countings" here
      if DWord ((Index + 1) * N_b) < N_M then
         Current_Tweak (0 .. 7) := Current_Tweak (0 .. 7) +
                                   (Index + 1) * N_b;
      --Current_Tweak(0..7) := Natural_To_Bytes((Index+1)*N_b,8);
      else
         --Current_Tweak(0..7) := Skeinword_To_Bytes(Skeinword(N_M));
         Current_Tweak (0 .. 7) := Current_Tweak (0 .. 7) + Natural (N_M);
      end if;

      --if its the first computation in this UBI we set bit 126 in the 128Bit
      --Tweak
      --this is the last but one in the last byte of the Tweak Block
      Set_Bit (Current_Tweak (Current_Tweak'Last), 6, --bit 126 of the total
        First_Run);

      --if its the last computation in this UBI we set bit 127 in the 128 Bit
      --Tweak
      --this is the last Bit in the last byte
      --if there was any BIT-Padding, we also set Bit 119 ---
      Set_Bit (Current_Tweak (Current_Tweak'Last), 7, --bit 127 of the total
        Last_Run);

      Set_Bit (Current_Tweak (Current_Tweak'Last - 1), 7,   --bit 119of the
                                                            --total
        Last_Run and B);

      --we are done and can return the value
      return Current_Tweak;
   end Get_Current_Tweak;

   procedure Straight_UBI
     (Mode              : in Skein_Mode;
      G                 : in Bytes;    --starting value on N_B Bytes
      Full_Message      : in Bytes;    --Message of variable lenght
      Full_Message_Bits : in Natural;  --the length of the input Message in
                                       --Bits
      T_S               : in Bytes;    --Starting Tweak T_S of 16 Byte
      Result            : out Bytes)   --the result of UBI:
   is
      Current_Tweak   : Bytes := T_S;  --we will manipulate this in every
                                       --UBI-round
      Current_Message : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1);
      --the current 8,16,24 Bytes

      Current_Key : Bytes := G;     --the "Keys" for the BlockCipher
                                    --as the first "Key" we use the G

      --here we store the current result of the block cipher
      --this is also holding the XOR of the blockcipher-result with message
      Current_Result : Bytes := Current_Key;    --this is correct for the
                                                --first step :)

      --we have to do a Bit-Padding if the input-length mod 8 != 0
      Bit_Padded_Message : constant Bytes :=
        Message_Bit_Padding
           (Message        => Full_Message,
            Desired_Length => Full_Message_Bits);
      --lets see if there was any Bit-Padding
      B : constant Boolean :=
        Get_Bit_Padding_Status
           (Message        => Full_Message,
            Desired_Length => Full_Message_Bits);
      --do the padding for the Message, so that we have a length mod N_b = 0
      --we do this here because we do not know how long the message will be
      --so we can initialize the unconstrained type Bytes right here..
      Byte_Padded_Message : constant Bytes :=
        Message_Byte_Padding
           (Mode             => Mode,
            Original_Message => Bit_Padded_Message);

      Bytes_Processed : Natural := 0;  --counter for the processed Bytes, we
                                       --need this for the
                                       --tweak-calculation and to know when to
                                       --finisch
      N_M : Natural := 0;
   begin
      --check if G has the correct Size
      if not (G'Length = Get_Number_Of_Skein_Bytes (Mode)) then
         Put_Line ("This is the wrong input size or mode, please check.");
         Put_Line
           (Integer'Image (G'Length) &
            " vs." &
            Integer'Image (Get_Number_Of_Skein_Bytes (Mode)));
         raise Program_Error;
      end if;

      N_M := Full_Message'Length;

      -- divide the Message to little pieces and process this
      while True loop
         Current_Tweak :=
            Get_Current_Tweak
              (T_S       => T_S,
               N_M       => DWord (N_M),  --we normaly dont
                                                         --need a special
                                                         --variable here!!!
               Index     => Bytes_Processed /
                            Get_Number_Of_Skein_Bytes (Mode),
               N_b       => Get_Number_Of_Skein_Bytes (Mode),
               First_Run => Bytes_Processed = 0,
               Last_Run  => Full_Message'Length - Bytes_Processed <=
                            Get_Number_Of_Skein_Bytes (Mode),
               B         => B);

         Current_Message :=
           Byte_Padded_Message (
           Bytes_Processed ..
           Bytes_Processed + Get_Number_Of_Skein_Bytes (Mode) - 1);

         Current_Key := Current_Result;

         --            Put_Line("We are starting threefish now "
         --                & Integer'Image(Current_Tweak'Length)
         --                & Integer'Image(Current_Message'Length)
         --                & Integer'Image(Current_Key'Length)
         --                );
         --now we have the tweak,key and ne message and can use the blockcipher
         threefish.Encrypt
           (Mode             => Mode,
            Block_Cipher_Key => Current_Key,
            Tweak            => Current_Tweak,
            Plaintext        => Current_Message,
            Result           => Current_Result);

         --as a final step lets XOR the result with the message
         for i in Current_Result'Range loop
            Current_Result (i) := Current_Result (i) xor Current_Message (i);
         end loop;

         --increment the counter for the next run
         Bytes_Processed := Bytes_Processed +
                            Get_Number_Of_Skein_Bytes (Mode);

         --if this was the last block we can finish this here
         exit when Bytes_Processed = Byte_Padded_Message'Length;
      end loop;
      --        Ada.Text_IO.Put_Line("Result: " & Integer'Image(Result'Length)
      --                             & "         Current_Result: " &
      --Integer'Image(Current_Result'Length));

      --maybe we should do:
      --for i in 0..Current_Result'Last loop
      --  Result(Result'First+i) := Current_Result(i);
      --end loop;
      Result := Current_Result;


   end Straight_UBI;



   procedure Output
     (Mode   : in Skein_Mode;
      G      : in Bytes;      --the chaining value
      N_0    : in Natural;    --number of required output BITS
      Result : out Bytes)     --the result, if N_0 mod 8 != 0
      --the last byte is only partially used
   is
      --we need at least the size for the current Mode
      Local_Result : Bytes (0 .. N_0 / 8 + Get_Number_Of_Skein_Bytes (Mode));
      --build N_b bytes more, we cut it later..
      Counter      : Natural         := 0;
      T_out        : constant Bytes (0 .. 15) := (15 => Byte (63), others => Byte (0));
   begin
      --we use the first N_0/8 Bytes
      while True loop
         Straight_UBI
           (Mode,
            G                 => G,
            Full_Message      => Natural_To_Bytes (Counter, 8),
            Full_Message_Bits => 8 * 8,
            T_S               => T_out,          --this is T_{out}* 2^{120}
            Result            =>
              Local_Result (
           Counter * Get_Number_Of_Skein_Bytes (Mode) ..
           Counter * Get_Number_Of_Skein_Bytes (Mode) +
           Get_Number_Of_Skein_Bytes (Mode) -
           1));

         Counter := Counter + 1;
         exit when Counter * Get_Number_Of_Skein_Bytes (Mode) >= N_0 / 8;
      end loop;

      --we just need the first N_0/8 Bytes
      if N_0 mod 8 = 0 then
         Result := Local_Result (0 .. N_0 / 8 - 1);
      else
         Result := Local_Result (0 .. N_0 / 8);       --one more byte
         --the user has to clearify whisch Bits he really wants to us
         --output are full Bytes always, never single Bits
      end if;
   end Output;

   function Get_Configuration_String
     (N_0  : in Natural;
      Y_l  : in Natural := 0;
      Y_f  : in Natural := 0;
      Y_m  : in Natural := 0)
      return Bytes
   is
      Conf_String : Bytes (0 .. 31) := (others => Byte (0));
   begin
      --shema identifier ASCII-String "SHA3" in Bytes 0..3
      Conf_String (3) := Byte (16#33#);
      Conf_String (2) := Byte (16#41#);
      Conf_String (1) := Byte (16#48#);
      Conf_String (0) := Byte (16#53#);
      --Conf_String(0..3) := Natural_To_Bytes(16#33414853#,4);

      --the version number
      Conf_String (4 .. 5) := Natural_To_Bytes (1, 2);

      --6..7 reserved - willbe zero for now

      --8..15 output-length in Bits
      Conf_String (8 .. 15) := Natural_To_Bytes (N_0, 8);

      -- 16 tree leaf size
      Conf_String (16 .. 16) := Natural_To_Bytes (Y_l, 1);
      -- 17 tree fan out enc.
      Conf_String (17 .. 17) := Natural_To_Bytes (Y_f, 1);
      -- 18 max tree height
      Conf_String (18 .. 18) := Natural_To_Bytes (Y_m, 1);

      -- 19..31 reserved, set to zero

      return Conf_String;
   end Get_Configuration_String;

   --the full Init call
   procedure Init
     (Mode  : in Skein_Mode;   --the Mode we want to use Skein (internal size)
      N_0   : in Natural;      --the desired output length in Bits
      K     : in Bytes;       --the Key for MAC-Mode
      Y_l   : in Natural;      --tree hashing leaf size
      Y_f   : in Natural;      --tree hashing fan-out
      Y_m   : in Natural;      --tree hashing maximim tree height
      State : out Bytes         --the outputof the init process
        )
   is
      --initialize K_Tick, we will set it to all-zero
      --or some value depending on K
      Empty_G : constant Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1) :=
        (others => Byte (0));
      K_Tick  : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1);
      T_Key   : constant Bytes (0 .. 15) := (others => Byte (0));

      --data used for the Configuration calculations
      C      : constant Bytes (0 .. 31) :=
        Get_Configuration_String
           (N_0 => N_0,
            Y_l => Y_l,
            Y_f => Y_f,
            Y_m => Y_m);
      T_Conf : constant Bytes (0 .. 15) := (15 => Byte (4), others => Byte (0));
   begin
      --calculate K_Tick
      if K'Length = 0 then                --This isnt correct, this is not
                                          --possible in Ada.
         K_Tick := (others => Byte (0));
      else
         Straight_UBI
           (Mode              => Mode,
            G                 => Empty_G,
            Full_Message      => K,
            Full_Message_Bits => K'Length * 8,
            T_S               => T_Key,
            Result            => K_Tick);
      end if;

      --now we have K_Tick and can go on with the Configuration calculation
      Straight_UBI
        (Mode              => Mode,
         G                 => K_Tick,
         Full_Message      => C,
         Full_Message_Bits => 32 * 8,        --this is fix
         T_S               => T_Conf,
         Result            => State);
   end Init;

   --the Simple Init
   --we need to implement it this way because a default value
   --for the key can not be realized in the full-Init
   procedure Init
     (Mode  : in Skein_Mode;
      N_0   : in Natural;
      State : out Bytes)
   is
      K : constant Bytes (0 .. -1) := (others => Byte (0));
   begin
      --call the "long" Init
      Init
        (Mode  => Mode,
         N_0   => N_0,
         K     => K,
         Y_l   => 0,
         Y_f   => 0,
         Y_m   => 0,
         State => State);
   end Init;

   procedure Update
     (Mode           : in Skein_Mode;
      Old_State      : in Bytes;
      Message        : in Bytes;
      Message_Length : in Natural;
      Type_Value     : in Byte;
      Y_l            : in Natural;
      Y_f            : in Natural;
      Y_m            : in Natural;
      New_State      : out Bytes)
   is
      Tweak : constant Bytes (0 .. 15) := (15 => Type_Value, others => Byte (0));
      --we need this Tweak to compare it with the input-tweak
      --only if the input-tweak is the T_msg and at least one Y != 0
      --then we use tree-hashing
      T_Msg : constant Bytes (0 .. 15) := (15 => Byte (48), others => Byte (0));
   begin
      --!!!!!!!!!!! we have to differ between normal UBI and TreeUBI here
      if Tweak = T_Msg and
         ((not (Y_l = 0) or not (Y_f = 0) or not (Y_m = 0)))
      then
         null;

      else
         --we want straight UBI
         Straight_UBI
           (Mode              => Mode,
            G                 => Old_State,
            Full_Message      => Message,         --the current message
            Full_Message_Bits => Message_Length,   --the current message
                                                   --length in bits
            T_S               => Tweak,           --the current tweak
            Result            => New_State);
      end if;
   end Update;

   --the simplified Update function
   procedure Update
     (Mode           : in Skein_Mode;
      Old_State      : in Bytes;
      Message        : in Bytes;
      Message_Length : in Natural;
      New_State      : out Bytes)
   is
      Type_Value_Msg : constant Byte := Byte (48);
   begin
      --call the full Update, with the parameters set to defaults
      --we need to do it this way because a default of T_Msg isnt possible
      Update
        (Mode           => Mode,
         Old_State      => Old_State,
         Message        => Message,
         Message_Length => Message_Length,
         Type_Value     => Type_Value_Msg,
         Y_l            => 0,
         Y_f            => 0,
         Y_m            => 0,
         New_State      => New_State);
   end Update;

   procedure Final
     (Mode      : in Skein_Mode;
      Old_State : in Bytes;
      N_0       : in Natural;
      New_State : out Bytes)
   is
      --the final result , the size is correct only for N_0 mod 8 = 0!!
      H : Bytes (0 .. N_0 / 8 - 1) := (others => Byte (0));
   begin
      --as a last step we do the Output-function
      Output (Mode => Mode, G => Old_State, N_0 => N_0, Result => H);

      --fill the output byte per Byte with the result
      for i in New_State'Range loop
         New_State (i) := H (i);
      end loop;
   end Final;

   procedure Hash
     (Mode        : in Skein_Mode;
      N_0         : in Natural;
      K           : in Bytes;
      Y_l         : in Natural;
      Y_f         : in Natural;
      Y_m         : in Natural;
      Tuple_Array : in Skein_Message_Tweak_Tuple_Pointer_Array;
      Result      : out Bytes)
   is
      Initresult   : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1) :=
        (others => Byte (0));
      Updateresult : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1) :=
        (others => Byte (0));
   begin
      Init
        (Mode  => Mode,
         N_0   => N_0,
         K     => K,
         Y_l   => Y_l,
         Y_f   => Y_f,
         Y_m   => Y_m,
         State => Initresult);

      for i in Tuple_Array'Range loop
         Update
           (Mode           => Mode,
            Old_State      => Initresult,
            Message        => Tuple_Array (i).all.Message,
            Message_Length => Tuple_Array (i).all.Message_Length,
            Type_Value     => Tuple_Array (i).all.Type_Value,
            Y_l            => Y_l,
            Y_f            => Y_f,
            Y_m            => Y_m,
            New_State      => Updateresult);
      end loop;

      Final
        (Mode      => Mode,
         Old_State => Updateresult,
         N_0       => N_0,
         New_State => Result);
   end Hash;

   procedure Hash
     (Mode           : in Skein_Mode;
      N_0            : in Natural;
      K              : in Bytes;
      Y_l            : in Natural;
      Y_f            : in Natural;
      Y_m            : in Natural;
      Message        : in Bytes;
      Message_Length : in Natural;
      Type_Value     : in Byte;
      Result         : out Bytes)
   is
      Initresult   : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1) :=
        (others => Byte (0));
      Updateresult : Bytes (0 .. Get_Number_Of_Skein_Bytes (Mode) - 1) :=
        (others => Byte (0));
   begin
      Init
        (Mode  => Mode,
         N_0   => N_0,
         K     => K,
         Y_l   => Y_l,
         Y_f   => Y_f,
         Y_m   => Y_m,
         State => Initresult);

      Update
        (Mode           => Mode,
         Old_State      => Initresult,
         Message        => Message,
         Message_Length => Message_Length,
         Type_Value     => Type_Value,
         Y_l            => Y_l,
         Y_f            => Y_f,
         Y_m            => Y_m,
         New_State      => Updateresult);

      Final
        (Mode      => Mode,
         Old_State => Updateresult,
         N_0       => N_0,
         New_State => Result);
   end Hash;

   procedure Skein_Complete
     (Mode           : in Skein_Mode;
      Output_Length_Bits            : in Natural;
      Message        : in Bytes;
      Message_Length_Bits : in Natural;
      Result         : out Bytes)
   is
      K_empty        : constant Bytes (0 .. -1) := (others => Byte (0));
      Type_Value_Msg : constant Byte              := Byte (48);
   begin
      --just call the full Hash with empty Key
      --and tree vaiables set to zero
      Hash
        (Mode           => Mode,
         N_0            => Output_Length_Bits,
         K              => K_empty,
         Y_l            => 0,
         Y_f            => 0,
         Y_m            => 0,
         Message        => Message,
         Message_Length => Message_Length_Bits,
         Type_Value     => Type_Value_Msg,
         Result         => Result);

   end Skein_Complete;

begin

   null;
end Crypto.Symmetric.Algorithm.Skein;
