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

--with skein_nodebug;         use skein_nodebug;
with Ada.Characters.Handling;               use Ada.Characters.Handling;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;			    use Ada.Text_IO;

package body Crypto.Symmetric.Algorithm.Threefish is

   Mix_Constants_256 : constant array (0 .. 7, 0 .. 1, 0 .. 2) of Natural :=
     (((0, 1, 14), (2, 3, 16)),
      ((0, 3, 52), (2, 1, 57)),
      ((0, 1, 23), (2, 3, 40)),
      ((0, 3, 5), (2, 1, 37)),
      ((0, 1, 25), (2, 3, 33)),
      ((0, 3, 46), (2, 1, 12)),
      ((0, 1, 58), (2, 3, 22)),
      ((0, 3, 32), (2, 1, 32)));

   Mix_Constants_512 : constant array (0 .. 7, 0 .. 3, 0 .. 2) of Natural :=
     (((0, 1, 46), (2, 3, 36), (4, 5, 19), (6, 7, 37)),
      ((2, 1, 33), (4, 7, 27), (6, 5, 14), (0, 3, 42)),
      ((4, 1, 17), (6, 3, 49), (0, 5, 36), (2, 7, 39)),
      ((6, 1, 44), (0, 7, 9), (2, 5, 54), (4, 3, 56)),
      ((0, 1, 39), (2, 3, 30), (4, 5, 34), (6, 7, 24)),
      ((2, 1, 13), (4, 7, 50), (6, 5, 10), (0, 3, 17)),
      ((4, 1, 25), (6, 3, 29), (0, 5, 39), (2, 7, 43)),
      ((6, 1, 8), (0, 7, 35), (2, 5, 56), (4, 3, 22)));

   Mix_Constants_1024 : constant array (0 .. 7, 0 .. 7, 0 .. 2) of Natural :=
     (((0, 1, 24),
       (2, 3, 13),
       (4, 5, 8),
       (6, 7, 47),
       (8, 9, 8),
       (10, 11, 17),
       (12, 13, 22),
       (14, 15, 37)),

      ((0, 9, 38),
       (2, 13, 19),
       (6, 11, 10),
       (4, 15, 55),
       (10, 7, 49),
       (12, 3, 18),
       (14, 5, 23),
       (8, 1, 52)),

      ((0, 7, 33),
       (2, 5, 4),
       (4, 3, 51),
       (6, 1, 13),
       (12, 15, 34),
       (14, 13, 41),
       (8, 11, 59),
       (10, 9, 17)),

      ((0, 15, 5),
       (2, 11, 20),
       (6, 13, 48),
       (4, 9, 41),
       (14, 1, 47),
       (8, 5, 28),
       (10, 3, 16),
       (12, 7, 25)),

      ((0, 1, 41),
       (2, 3, 9),
       (4, 5, 37),
       (6, 7, 31),
       (8, 9, 12),
       (10, 11, 47),
       (12, 13, 44),
       (14, 15, 30)),

      ((0, 9, 16),
       (2, 13, 34),
       (6, 11, 56),
       (4, 15, 51),
       (10, 7, 4),
       (12, 3, 53),
       (14, 5, 42),
       (8, 1, 41)),

      ((0, 7, 31),
       (2, 5, 44),
       (4, 3, 47),
       (6, 1, 46),
       (12, 15, 19),
       (14, 13, 42),
       (8, 11, 44),
       (10, 9, 25)),

      ((0, 15, 9),
       (2, 11, 48),
       (6, 13, 35),
       (4, 9, 52),
       (14, 1, 23),
       (8, 5, 31),
       (10, 3, 37),
       (12, 7, 20)));


   function "+" (Left : DWord; Right : Integer) return DWord is
   begin
      return Left + DWord (Right);
   end "+";

   function Natural_To_Bytes (N : Natural; number : Natural) return Bytes is
      result : Bytes (0 .. number - 1) := (others => Byte (0));
   begin
      for i in result'Range loop  --Natural can be at least 256**3
         --result(i) := Byte( ( N/(256**i) ) mod 256);
         if i < 4 then
            result (i) := Byte ((N / (256 ** i)) mod 256);
            --Ada.Text_IO.Put_Line(Show_Hex(result(i)));
         end if;
      end loop;
      return result;
   end Natural_To_Bytes;

   function Bytes_To_Dword (b : in Bytes) return DWord is
      My_SW : DWord := DWord (0);
   begin
      if not (b'Length = 8) then
         Put_Line
           ("The Length of Bytes must be 8 for converting to Dword");
         raise Program_Error;
      end if;
      --8 Bytes are one word,
      for j in b'Range loop
         --we need mod 8 here because in call from Bytes_To_Dwords
         --the indices are kept :/
         My_SW := My_SW + (DWord (b (j)) * 256 ** (j mod 8));
      end loop;
      return My_SW;
   end Bytes_To_Dword;

   --8 bytes are one Dword
   function Bytes_To_Dwords (b : in Bytes) return DWords is
      My_SW_Array : DWords (0 .. b'Length / 8 - 1);
   begin
      if not (b'Length mod 8 = 0) then
         Put_Line ("The Length of Bytes must be a multiple of 8");
         Put_Line (Integer'Image (b'Length));
         raise Program_Error;
      end if;
      for i in My_SW_Array'Range loop
         My_SW_Array (i) := Bytes_To_Dword (b (8 * i .. 8 * i + 7));
      end loop;
      return My_SW_Array;
   end Bytes_To_Dwords;

   --convert one single Dword to an array of 8 Bytes
   function Dword_To_Bytes (s : in DWord) return Bytes is
      My_Bytes_Array : Bytes (0 .. 7);
   begin
      for i in My_Bytes_Array'Range loop
         My_Bytes_Array (i) := Byte (s / 256 ** i mod 256);
      end loop;
      return My_Bytes_Array;
   end Dword_To_Bytes;

   --one Dword is 8 Bytes
   function Dwords_To_Bytes (s : in DWords) return Bytes is
      My_Bytes : Bytes (0 .. s'Length * 8 - 1);
   begin
      for i in s'Range loop
         My_Bytes (i * 8 .. i * 8 + 7) := Dword_To_Bytes (s (i));
      end loop;
      return My_Bytes;
   end Dwords_To_Bytes;

   function "+" (Left : Bytes; Right : Natural) return Bytes is
   begin
      if not (Left'Length = 8) then
         Put_Line ("maximum of 8 Byte is allowed for Addition");
         raise Program_Error;
      end if;
      return Dword_To_Bytes (Bytes_To_Dword (Left) + Right);
   end "+";

   function Get_Number_Of_Skein_Bytes (Mode : in Skein_Mode) return Natural is
   begin
      case Mode is
         when m256 =>
            return 32;
         when m512 =>
            return 64;
         when m1024 =>
            return 128;
      end case;
   end Get_Number_Of_Skein_Bytes;

   function Get_Last_Word_Index (mode : in Threefish_Mode) return Natural is
   begin
      case mode is
         when mode256 =>
            return 256 / 64 - 1;
         when mode512 =>
            return 512 / 64 - 1;
         when mode1024 =>
            return 1024 / 64 - 1;
      end case;
   end Get_Last_Word_Index;

   function Get_Name (mode : in Threefish_Mode) return String is
   begin
      case mode is
         when mode256 =>
            return "Threefish-256";
         when mode512 =>
            return "Threefish-512";
         when mode1024 =>
            return "Threefish-1024";
      end case;
   end Get_Name;

   function Get_Bit_Count (mode : in Threefish_Mode) return Natural is
   begin
      case mode is
         when mode256 =>
            return 256;
         when mode512 =>
            return 512;
         when mode1024 =>
            return 1024;
      end case;
   end Get_Bit_Count;

   function Get_Number_Of_Rounds (mode : in Threefish_Mode) return Natural is
   begin
      case mode is
         when mode256 =>
            return 72;
         when mode512 =>
            return 72;
         when mode1024 =>
            return 80;
      end case;
   end Get_Number_Of_Rounds;

   function Get_Number_Of_Mix_Operations
     (Mode : in Threefish_Mode)
      return Natural
   is
   begin
      case Mode is
         when mode256 =>
            return 2;
         when mode512 =>
            return 4;
         when mode1024 =>
            return 8;
      end case;
   end Get_Number_Of_Mix_Operations;

   function Skein_Mode_To_Threefish_Mode
     (SMode : in Skein_Mode)
      return  Threefish_Mode
   is
   begin
      case SMode is
         when m256 =>
            return Threefish.mode256;
         when m512 =>
            return Threefish.mode512;
         when m1024 =>
            return Threefish.mode1024;
      end case;
   end Skein_Mode_To_Threefish_Mode;

   function Threefish_Mode_Check
     (mode   : Threefish_Mode;
      keys   : Threefish_Keys'Class;
      tweaks : Threefish_Tweaks'Class)
      return   Boolean
   is
      pragma Unreferenced (tweaks);
   begin
      return keys.data'Last = Get_Last_Word_Index (mode);
   end Threefish_Mode_Check;

   procedure Encrypt
     (Mode             : in Skein_Mode;
      Block_Cipher_Key : in Bytes;
      Tweak            : in Bytes;
      Plaintext        : in Bytes;
      Result           : out Bytes)
   is
      words    : constant Threefish_Words'Class  :=
        Make_Words
           (mode => Threefish.Skein_Mode_To_Threefish_Mode (Mode),
            SWA  => Bytes_To_Dwords (Plaintext));
      Keys     : constant Threefish_Keys'Class   :=
        Make_Keys
           (mode => Threefish.Skein_Mode_To_Threefish_Mode (Mode),
            SWA  => Bytes_To_Dwords (Block_Cipher_Key));
      tweaks   : constant Threefish_Tweaks'Class :=
        Make_Tweaks
           (mode => Threefish.Skein_Mode_To_Threefish_Mode (Mode),
            SWA  => Bytes_To_Dwords (Tweak));
      outwords : Threefish_Words'Class  :=
        Make_Words (Skein_Mode_To_Threefish_Mode (Mode));
   begin
      --Show only the inputs
      --Show_Words(true,"inputwords",words);

      Encrypt
        (Mode      => Skein_Mode_To_Threefish_Mode (Mode),
         Inwords   => words,
         Keys      => Keys,
         Tweaks    => tweaks,
         Outwords  => outwords,
         Talk_Mode => False);
      Result := Dwords_To_Bytes (outwords.data);

      --Show_Words(true,"results",outwords);
   end Encrypt;

   procedure Encrypt
     (Mode      : in Threefish_Mode;
      Inwords   : in Threefish_Words'Class;
      Keys      : in Threefish_Keys'Class;
      Tweaks    : in Threefish_Tweaks'Class;
      Outwords  : out Threefish_Words'Class;
      Talk_Mode : in Boolean := False)
   is
      pragma Unreferenced (Talk_Mode);

      --we calculate the Extended Keys here (KeyShedule)
      Extended_Keys : Threefish_Extended_Keys (Get_Last_Word_Index (Mode));

      --we have to count the rounds and the keyinjections

      --some vaiales needed for the MIX-operations
      TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND : constant Natural :=
        Get_Number_Of_Mix_Operations (Mode);
      TOTAL_NUMBER_OF_ROUNDS                   : constant Natural :=
        Get_Number_Of_Rounds (Mode);
      Current_Mix_Variable                     : Threefish_Mix_Variables_Type;

   begin




      --do the Keyshedule
      Key_Schedule (Mode, Keys, Tweaks, Extended_Keys);



      -- now we have the Extended Keys
      -- we can do the Keyinjection and the Mixing
      Outwords := Inwords;

      --the initial keyInjection
      Key_Injection (Outwords, Extended_Keys, 0);

      --we want to do 72/80 rounds
      --insert the Extended Keys after every 4th round
      --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
      for r in 1 .. TOTAL_NUMBER_OF_ROUNDS loop
         for j in 1 .. TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
            --get the values for the current MIX opeation
            Current_Mix_Variable := Get_Mix_Variables (Mode, r, j);

            --do the mixing
            Threefish_mix
              (Outwords.data (Current_Mix_Variable.Input1),
               Outwords.data (Current_Mix_Variable.Input2),
               Current_Mix_Variable.Rotconst);
         end loop;

         --do the keyinjection afte every round mod 4 = 0
         if r mod 4 = 0 then
            Key_Injection (Outwords, Extended_Keys, r / 4);

         end if;

      end loop;

   end Encrypt;

   --ATTENTION:
   --we need a new KeyDeInjection
   --we need a new DeMIX
   procedure Decrypt
     (mode      : in Threefish_Mode;
      inwords   : in Threefish_Words'Class;
      keys      : in Threefish_Keys'Class;
      tweaks    : in Threefish_Tweaks'Class;
      outwords  : out Threefish_Words'Class;
      Talk_Mode : in Boolean := False)
   is
      pragma Unreferenced (Talk_Mode);
      --we calculate the Extended Keys here (KeyShedule)
      Extended_Keys : Threefish_Extended_Keys (Get_Last_Word_Index (mode));

      --some vaiales needed for the MIX-operations
      TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND : constant Natural :=
        Get_Number_Of_Mix_Operations (mode);
      TOTAL_NUMBER_OF_ROUNDS                   : constant Natural :=
        Get_Number_Of_Rounds (mode);
      Current_Mix_Variable                     : Threefish_Mix_Variables_Type;
   begin

      --do the Keyshedule
      Key_Schedule (mode, keys, tweaks, Extended_Keys);

      -- now we have the Extended Keys
      -- we can do the Keyinjection and the "unMixing"
      outwords := inwords;

      --we want to do 72/80 rounds
      --insert the Extended Keys after every 4th round
      --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
      for r in reverse 1 .. TOTAL_NUMBER_OF_ROUNDS loop

         --do the keyinjection after every round mod 4 = 0
         if r mod 4 = 0 then
            Reverse_Key_Injection (outwords, Extended_Keys, r / 4);
         end if;

         for j in 1 .. TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
            --get the values for the current MIX opeation
            Current_Mix_Variable := Get_Mix_Variables (mode, r, j);

            --do the mixing
            Threefish_Reverse_mix
              (outwords.data (Current_Mix_Variable.Input1),
               outwords.data (Current_Mix_Variable.Input2),
               Current_Mix_Variable.Rotconst);
         end loop;

      end loop;

      --the "initial" keyInjection
      Reverse_Key_Injection (outwords, Extended_Keys, 0);
   end Decrypt;

   procedure Key_Schedule
     (mode     : in Threefish_Mode;
      keys     : in Threefish_Keys'Class;
      Tweaks   : in Threefish_Tweaks'Class;
      ext_Keys : in out Threefish_Extended_Keys'Class)
   is
      --some variables we will need more often
      k_f : constant Natural := keys.data'First;
      k_l : constant Natural := keys.data'Last;

      --the number of words(just for better readability
      n_w : constant Natural := k_l + 1;

      --we need longer Keys and tweaks for the calculation
      long_Keys   : DWords (0 .. k_l + 1);
      long_tweaks : DWords (0 .. 2);

      --some Dwords we will need
      c240 : constant DWord := Create ("1BD11BDAA9FC1A22", Hex);
   begin
      --------------------------
      --first we have fill the longer Keys and tweaks
      long_Keys (0 .. k_l) := keys.data (0 .. k_l);
      long_Keys (n_w)      := c240;
      for i in k_f .. k_l loop
         long_Keys (n_w) := long_Keys (n_w) xor keys.data (i);
      end loop;

      long_tweaks (0 .. 1) := Tweaks.data (0 .. 1);
      long_tweaks (2)      := Tweaks.data (0) xor Tweaks.data (1);

      --now we can calculate the whole Extended Keys matrix
      for r in 0 .. Get_Number_Of_Rounds (mode) / 4 loop
         for i in 0 .. n_w - 4 loop
            ext_Keys.data (r, i) := long_Keys ((r + i) mod (n_w + 1));
         end loop;

         ext_Keys.data (r, n_w - 3) :=
           long_Keys ((r + n_w - 3) mod (n_w + 1)) +
           long_tweaks (r mod 3);
         ext_Keys.data (r, n_w - 2) :=
           long_Keys ((r + n_w - 2) mod (n_w + 1)) +
           long_tweaks ((r + 1) mod 3);
         ext_Keys.data (r, n_w - 1) :=
           long_Keys ((r + n_w - 1) mod (n_w + 1)) + DWord (r);
      end loop;


   end Key_Schedule;

   procedure Key_Injection
     (words : in out Threefish_Words'Class;
      ks    : in Threefish_Extended_Keys'Class;
      r     : in Natural)
   is
   begin
      --we just have to ADD words and Extended Keys for the current round
      for w in words.data'Range loop
         words.data (w) := words.data (w) + ks.data (r, w);
      end loop;
   end Key_Injection;

   procedure Reverse_Key_Injection
     (words : in out Threefish_Words'Class;
      ks    : in Threefish_Extended_Keys'Class;
      r     : in Natural)
   is
   begin
      --we just have to ADD words and Extended Keys for the current round
      for w in words.data'Range loop
         words.data (w) := words.data (w) - ks.data (r / 8, w);
      end loop;
   end Reverse_Key_Injection;

   procedure Threefish_mix
     (sw1      : in out DWord;
      sw2      : in out DWord;
      rotConst : in Natural)
   is
   begin
      sw1 := sw1 + sw2;
      sw2 := Rotate_Left (sw2, rotConst);
      sw2 := sw1 xor sw2;
   end Threefish_mix;

   procedure Threefish_Reverse_mix
     (sw1      : in out DWord;
      sw2      : in out DWord;
      rotConst : in Natural)
   is
   begin
      sw2 := sw2 xor sw1;
      sw2 := Rotate_Left (sw2, 64 - rotConst); --in fact we want an right_rot

      sw1 := sw1 - sw2;
   end Threefish_Reverse_mix;

   -------------------------------------------
   --the helper functions
   -------------------------------------------
   function Get_Mix_Variables
     (Mode           : in Threefish_Mode;
      Roundnumber    : in Natural;
      Mix_Fct_Number : in Natural)
      return           Threefish_Mix_Variables_Type
   is
      my_Mix_Variable : Threefish_Mix_Variables_Type;
   begin
      case Mode is
         when mode256 =>
            --thanks to the ads we have to add 1 to the indexes
            --maybe we change this later, then we only have to do it here
            my_Mix_Variable.Input1   :=
              Mix_Constants_256 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 0
);
            my_Mix_Variable.Input2   :=
              Mix_Constants_256 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 1
);
            my_Mix_Variable.Rotconst :=
              Mix_Constants_256 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 2
);

         when mode512 =>
            my_Mix_Variable.Input1   :=
              Mix_Constants_512 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 0
);
            my_Mix_Variable.Input2   :=
              Mix_Constants_512 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 1
);
            my_Mix_Variable.Rotconst :=
              Mix_Constants_512 ((Roundnumber - 1) mod 8, Mix_Fct_Number - 1, 2
);

         when mode1024 =>
            my_Mix_Variable.Input1   :=
              Mix_Constants_1024 ((Roundnumber - 1) mod 8, Mix_Fct_Number -
                                                           1, 0);
            my_Mix_Variable.Input2   :=
              Mix_Constants_1024 ((Roundnumber - 1) mod 8, Mix_Fct_Number -
                                                           1, 1);
            my_Mix_Variable.Rotconst :=
              Mix_Constants_1024 ((Roundnumber - 1) mod 8, Mix_Fct_Number -
                                                           1, 2);
      end case;

      return my_Mix_Variable;
   end Get_Mix_Variables;

   -------
   function Make_Words (mode : Threefish_Mode) return Threefish_Words'Class is
      words : Threefish_Words (Last_Index => Get_Last_Word_Index (mode));
   begin
      for i in words.data'Range loop
         words.data (i) := Create ("000", Hex);
      end loop;
      return words;
   end Make_Words;
   --------
   function Make_Keys (mode : Threefish_Mode) return Threefish_Keys'Class is
      Keys : Threefish_Keys (Get_Last_Word_Index (mode));
   begin
      for i in Keys.data'Range loop
         Keys.data (i) := Create ("000", Hex);
      end loop;
      return Keys;
   end Make_Keys;
   --------
   function Make_Tweaks
     (mode : Threefish_Mode)
      return Threefish_Tweaks'Class
   is
      tweaks : Threefish_Tweaks (Get_Last_Word_Index (mode));
   begin
      for i in tweaks.data'Range loop
         tweaks.data (i) := Create ("000", Hex);
      end loop;
      return tweaks;
   end Make_Tweaks;
   ---------
   function Make_Extended_Keys
     (mode : Threefish_Mode)
      return Threefish_Extended_Keys'Class
   is
      Extended_Keys : Threefish_Extended_Keys (Get_Last_Word_Index (mode));
   begin
      for i in Extended_Keys.data'Range (1) loop
         for j in Extended_Keys.data'Range (2) loop
            Extended_Keys.data (i, j) := Create ("000", Hex);
         end loop;
      end loop;
      return Extended_Keys;
   end Make_Extended_Keys;
   ----------
   function Make_Words
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Words'Class
   is
      words : Threefish_Words (Last_Index => Get_Last_Word_Index (mode));
   begin
      if not (SWA'Last = Get_Last_Word_Index (mode)) then
         Put_Line ("Wrong Range, please check");
         raise Program_Error;
      end if;
      for i in words.data'Range loop
         words.data (i) := SWA (i);
      end loop;
      return words;
   end Make_Words;
   ---------------
   function Make_Keys
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Keys'Class
   is
      Keys : Threefish_Keys (Last_Index => Get_Last_Word_Index (mode));
   begin
      if not (SWA'Last = Get_Last_Word_Index (mode)) then
         Put_Line ("Wrong Range, please check");
         raise Program_Error;
      end if;
      for i in Keys.data'Range loop
         Keys.data (i) := SWA (i);
      end loop;
      return Keys;
   end Make_Keys;
   -------------
   function Make_Tweaks
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Tweaks'Class
   is
      tweaks : Threefish_Tweaks (Last_Index => Get_Last_Word_Index (mode));
   begin
      if not (SWA'Last = 1) then
         Put_Line
           ("Wrong Range for Tweaks, please check:" &
            Integer'Image (SWA'Last) &
            " vs." &
            Integer'Image (Get_Last_Word_Index (mode)));
         raise Program_Error;
      end if;
      for i in tweaks.data'Range loop
         tweaks.data (i) := SWA (i);
      end loop;
      return tweaks;
   end Make_Tweaks;


   procedure Set_Threefish_Word
     (words : in out Threefish_Words'Class;
      Index : in Natural;
      Word  : in DWord)
   is
   begin
      words.data (Index) := Word;
   end Set_Threefish_Word;

   procedure Set_Threefish_Key
     (keys  : in out Threefish_Keys'Class;
      Index : in Natural;
      Key   : in DWord)
   is
   begin
      keys.data (Index) := Key;
   end Set_Threefish_Key;

   procedure Set_Threefish_Tweak
     (tweaks : in out Threefish_Tweaks'Class;
      Index  : in Natural;
      Tweak  : in DWord)
   is
   begin
      tweaks.data (Index) := Tweak;
   end Set_Threefish_Tweak;

   function Get_Threefish_Word
     (Words : in Threefish_Words'Class;
      Index : in Natural)
      return  DWord
   is
   begin
      return Words.data (Index);
   end Get_Threefish_Word;

   function Get_Threefish_Key
     (Keys  : in Threefish_Keys'Class;
      Index : in Natural)
      return  DWord
   is
   begin
      return Keys.data (Index);
   end Get_Threefish_Key;

   function Get_Threefish_Tweak
     (Tweaks : in Threefish_Tweaks'Class;
      Index  : in Natural)
      return   DWord
   is
   begin
      return Tweaks.data (Index);
   end Get_Threefish_Tweak;

   ------------Types & Functions from old Types-Skein.adb

   --we need this for the random Dwords
   subtype Random_Type is Integer range 0 .. 15;
   package Random_Pack is new Ada.Numerics.Discrete_Random (Random_Type);
   G : Random_Pack.Generator;

   function Generate_Number return Integer is
   begin
      --Random_Pack.Reset (G);
      return Random_Pack.Random (G);
   end Generate_Number;

   function Create
     (Input : in String;
      Mode  : in Dword_Input_Mode_Type := Hex)
      return  DWord
   is

      output     : DWord            := 0;
      Input_Copy : Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Input);

      --needed to fill missing lengths
      Full_Hex_String : String (1 .. 16);
      Full_Bin_String : String (1 .. 64);

   begin

      --first lets remove all delimiters TODO
      remove_Delimiters (Input_Copy);

      case Mode is
         when Hex =>
            --fill missing inputs with zeros / cut too long inputs
            for i in reverse 1 .. 16 loop
               --Put_Line("i: " & i'Img & "    input'Last: " & input'Last'Img);
               if i >
                  Ada.Strings.Unbounded.To_String (Input_Copy)'Last
               then
                  Full_Hex_String (17 - i) := '0';
               else
                  --fill with correct entry from input
                  Full_Hex_String (17 - i) :=
                    Ada.Strings.Unbounded.To_String (Input_Copy) (
                    Ada.Strings.Unbounded.To_String (Input_Copy)'Last +
                                                                  1 -
                                                                  i);
               end if;
            end loop;
            --Ada.Text_IO.Put_LIne("The full Hex String: " & Full_Hex_String);
            for i in Full_Hex_String'Range loop --attention, Strings are in
                                                --Range 1..xxx
               output := output +
                         DWord (get_Value_From_Hex_Char
                                   (Full_Hex_String (i))) *
                         16 ** (17 - i - 1);
            end loop;

         when Bin =>
            --fill missing inputs with zeros / cut too long inputs
            --all Strings start from index 1
            for i in reverse 1 .. 64 loop
               if i >
                  Ada.Strings.Unbounded.To_String (Input_Copy)'Last
               then    --if the input is to
                  --short
                  Full_Bin_String (65 - i) := '0';
               else
                  --TODO: check if its '1' or '0' and check for spacers

                  Full_Bin_String (65 - i) :=
                    Ada.Strings.Unbounded.To_String (Input_Copy) (
                    Ada.Strings.Unbounded.To_String (Input_Copy)'Last +
                                                                  1 -
                                                                  i);
               end if;
            end loop;

            --now lets create the word
            --Put_Line("Create was called for binmode");
            for i in reverse Full_Bin_String'Range loop  --attention, Strings
                                                         --are in Range 1..xxx
               output := output +
                         get_Value_From_Bin_Char (Full_Bin_String (i)) *
                         2 ** (65 - i - 1);
            end loop;
      end case;

      return output;
   end Create;

   function Create (Mode : Dword_Kind_Mode_Type) return DWord is
      result            : DWord;
      Result_Hex_String : String                       := "0000000000000000";
      Hex_Array         : constant array (0 .. 15) of Character :=
        ('0',
         '1',
         '2',
         '3',
         '4',
         '5',
         '6',
         '7',
         '8',
         '9',
         'A',
         'B',
         'C',
         'D',
         'E',
         'F');
   begin
      case Mode is
         when random =>
            --get a random number between 0 and 15
            --rand.reset(my_Generator);
            for i in Result_Hex_String'Range loop
               Result_Hex_String (i) := Hex_Array (Generate_Number);
            end loop;

            --Ada.Text_IO.Put_Line("");
            --Ada.Text_IO.Put_Line(Result_Hex_String);
            result := Create (Result_Hex_String, Hex);
         when all_zero =>
            result := Create ("00000000", Hex);
         when all_one =>
            result := Create ("FFFFFFFF_FFFFFFFF", Hex);
      end case;
      return result;
   end Create;

   --Stuff further needed

   --removes all delimiters and spaces from a given unbounded String
   --makes everything lowercase
   procedure remove_Delimiters (text : in out Unbounded_String) is
      Last_Index : constant Natural                  :=
        Ada.Strings.Unbounded.To_String (text)'Last;
      new_text   : String (1 .. Last_Index) :=
        Ada.Strings.Unbounded.To_String (text);
      counter    : Natural                  := 1;
   begin
      for i in 1 .. Last_Index loop
         if (new_text (i) = ' ' or
             new_text (i) = '.' or
             new_text (i) = '_')
         then
            null;
         --Put_LIne("this was a delimiter");
         else
            new_text (counter) := new_text (i);
            counter            := counter + 1;
         end if;
      end loop;
      --make it lowercase
      new_text := To_Lower (new_text);
      --set the out-parameter
      text :=
         Ada.Strings.Unbounded.To_Unbounded_String
           (new_text (1 .. counter - 1));
   end remove_Delimiters;

   function get_Value_From_Hex_Char (Hex_Char : Character) return Natural is
   begin
      case Hex_Char is
      when '0' =>
         return 0;
      when '1' =>
         return 1;
      when '2' =>
         return 2;
      when '3' =>
         return 3;
      when '4' =>
         return 4;
      when '5' =>
         return 5;
      when '6' =>
         return 6;
      when '7' =>
         return 7;
      when '8' =>
         return 8;
      when '9' =>
         return 9;
      when 'a' =>
         return 10;
      when 'b' =>
         return 11;
      when 'c' =>
         return 12;
      when 'd' =>
         return 13;
      when 'e' =>
         return 14;
      when 'f' =>
         return 15;
      when others =>
         --TODO: set all zero, abort whole operation, raise exception
         Put_Line ("incorrect input for hex, please check");
         raise Program_Error;
      end case;
   end get_Value_From_Hex_Char;

   function get_Value_From_Bin_Char (Bin_Char : Character) return Natural is
   begin
      case Bin_Char is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when others =>
            --TODO: set all zero, abort whole operation, raise exception
            Put_Line ("incorrect input for bin, please check");
            raise Program_Error;
      end case;
   end get_Value_From_Bin_Char;

begin --package initialisation
   Random_Pack.Reset (G);
   null;
end Crypto.Symmetric.Algorithm.Threefish;
