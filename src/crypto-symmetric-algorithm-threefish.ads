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
with Crypto.Types;       use Crypto.Types;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
--
package Crypto.Symmetric.Algorithm.Threefish is



   ----------------------------------------------------------------
   --skein-specific tools imported from old types.skein
   ----------------------------------------------------------------

   --minor special type functions, used by Threefish and Skein



   function Natural_To_Bytes (N : Natural; number : Natural) return Bytes;

   function Bytes_To_Dword (b : in Bytes) return DWord;

   function Bytes_To_Dwords (b : in Bytes) return DWords;

   function Dword_To_Bytes (s : in DWord) return Bytes;

   function Dwords_To_Bytes (s : in DWords) return Bytes;

   --function "+" (Left : Bytes; Right : Natural) return Bytes;


   --the Mode type, just for distinguishing the diffrent mode of Skein
   type Skein_Mode is (m256, m512, m1024);
   function Get_Number_Of_Skein_Bytes (Mode : in Skein_Mode) return Natural;

   --the different supported modes of threefish
   type Threefish_Mode is (mode256, mode512, mode1024);

   --conversation function from Skein-Mode to Threefish-mode
   --the modes are only used to get the size of the internal state
   function Skein_Mode_To_Threefish_Mode
     (SMode : in Skein_Mode)
      return  Threefish_Mode;

   --returns the sizes of arrays for words and keys for the differnet modes
   function Get_Last_Word_Index (mode : in Threefish_Mode) return Natural;

   --returns the name of the version for a given mode
   function Get_Name (mode : in Threefish_Mode) return String;

   --returns the number of Bits used for words and keys for a given
   --threefisg-mode
   function Get_Bit_Count (mode : in Threefish_Mode) return Natural;

   --returns the number of total rounds for a given Threefish Mode
   --this is 72 for TF256 and TF-512, 80 for TF-1024
   function Get_Number_Of_Rounds (mode : in Threefish_Mode) return Natural;

   --returns the number of MIX-operations for the given Threefish Mode
   function Get_Number_Of_Mix_Operations
     (Mode : in Threefish_Mode)
      return Natural;

   --records holding array of skeinwords
   type Threefish_Words (Last_Index : Natural) is tagged private;
   type Threefish_Keys (Last_Index : Natural) is tagged private;
   type Threefish_Tweaks (Last_Index : Natural) is tagged private;
   type Threefish_State (Last_Index : Natural) is tagged private;
   type Threefish_Extended_Keys (Last_Index : Natural) is tagged private;


   --Status getter/setter

   procedure Set_Threefish_Word
     (words : in out Threefish_Words'Class;
      Index : in Natural;
      Word  : in DWord);

   procedure Set_Threefish_Key
     (keys  : in out Threefish_Keys'Class;
      Index : in Natural;
      Key   : in DWord);

   procedure Set_Threefish_Tweak
     (tweaks : in out Threefish_Tweaks'Class;
      Index  : in Natural;
      Tweak  : in DWord);

   function Get_Threefish_Word
     (Words : in Threefish_Words'Class;
      Index : in Natural)
      return  DWord;

   function Get_Threefish_Key
     (Keys  : in Threefish_Keys'Class;
      Index : in Natural)
      return  DWord;

   function Get_Threefish_Tweak
     (Tweaks : in Threefish_Tweaks'Class;
      Index  : in Natural)
      return   DWord;

   ----------------------------------------------------------------
   --wrapper function fo creating the words, keys and tweak blocks
   ----------------------------------------------------------------
   function Make_Words (mode : Threefish_Mode) return Threefish_Words'Class;
   function Make_Keys (mode : Threefish_Mode) return Threefish_Keys'Class;
   function Make_Tweaks
     (mode : Threefish_Mode)
      return Threefish_Tweaks'Class;
   function Make_Extended_Keys
     (mode : Threefish_Mode)
      return Threefish_Extended_Keys'Class;

   function Make_Words
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Words'Class;
   function Make_Keys
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Keys'Class;
   function Make_Tweaks
     (mode : Threefish_Mode;
      SWA  : DWords)
      return Threefish_Tweaks'Class;

   --type holding the rotation and permutation constants
   type Threefish_Mix_Variables_Type is tagged record
      Input1   : Natural;
      Input2   : Natural;
      Rotconst : Natural;
   end record;

   --returns the MIX-function data for a given round and MIX-function
   --data is read from the Constant arays defined at threefish.adb
   function Get_Mix_Variables
     (Mode           : in Threefish_Mode;
      Roundnumber    : in Natural;
      Mix_Fct_Number : in Natural)
      return           Threefish_Mix_Variables_Type;

   -------------------------------------------------
   --the main procedures of the whole blockcipher --
   -------------------------------------------------
   --inputs are arrays of Bytes
   --conversion from Byte-array to Skeinword is done here
   procedure Encrypt
     (Mode             : in Skein_Mode;
      Block_Cipher_Key : in Bytes;
      Tweak            : in Bytes;
      Plaintext        : in Bytes;
      Result           : out Bytes);

   --inputs are the specified types
   --this is the "real" procedure for encryption, the other ones are just
   --wrapper
   procedure Encrypt
     (Mode      : in Threefish_Mode;
      Inwords   : in Threefish_Words'Class;
      Keys      : in Threefish_Keys'Class;
      Tweaks    : in Threefish_Tweaks'Class;
      Outwords  : out Threefish_Words'Class;
      Talk_Mode : in Boolean := False);

   procedure Decrypt
     (mode      : in Threefish_Mode;
      inwords   : in Threefish_Words'Class;
      keys      : in Threefish_Keys'Class;
      tweaks    : in Threefish_Tweaks'Class;
      outwords  : out Threefish_Words'Class;
      Talk_Mode : in Boolean := False);

   procedure Key_Schedule
     (mode     : in Threefish_Mode;
      keys     : in Threefish_Keys'Class;
      Tweaks   : in Threefish_Tweaks'Class;
      ext_Keys : in out Threefish_Extended_Keys'Class);

   procedure Key_Injection
     (words : in out Threefish_Words'Class;
      ks    : in Threefish_Extended_Keys'Class;
      r     : in Natural);

   procedure Reverse_Key_Injection
     (words : in out Threefish_Words'Class;
      ks    : in Threefish_Extended_Keys'Class;
      r     : in Natural);

   procedure Threefish_mix
     (sw1      : in out DWord;
      sw2      : in out DWord;
      rotConst : in Natural);

   procedure Threefish_Reverse_mix
     (sw1      : in out DWord;
      sw2      : in out DWord;
      rotConst : in Natural);
   -------------------------------------
   --some helper functions
   -------------------------------------

   --check to see if the input has the correct lenght for the given mode
   --maybe we can throw a exception here later
   function Threefish_Mode_Check
     (mode   : Threefish_Mode;
      keys   : Threefish_Keys'Class;
      tweaks : Threefish_Tweaks'Class)
      return   Boolean;


private
   type Threefish_Words (Last_Index : Natural) is tagged record
      data : DWords (0 .. Last_Index);
   end record;

   type Threefish_Keys (Last_Index : Natural) is tagged record
      data : DWords (0 .. Last_Index);
   end record;

   type Threefish_Tweaks (Last_Index : Natural) is tagged record
      data : DWords (0 .. 1);
   end record;

   type Threefish_State (Last_Index : Natural) is tagged record
      words  : DWords (0 .. Last_Index);
      keys   : DWords (0 .. Last_Index);
      tweaks : DWords (0 .. 2);
   end record;

   type Dword_Matrix is array (Natural range <>, Natural range <>) of DWord;

   type Threefish_Extended_Keys (Last_Index : Natural) is tagged record
   --in fact we just need 18 entries for tf-256 and tf-512
      data : Dword_Matrix (0 .. 20, 0 .. Last_Index);
   end record;

   --type for supported Dword-inputs for Create-function with String input
   type Dword_Input_Mode_Type is (Hex, Bin);
   function Create
     (Input : in String;
      Mode  : in Dword_Input_Mode_Type := Hex)
      return  DWord;

   --we also want to be able to create defined Dwords
   type Dword_Kind_Mode_Type is (random, all_zero, all_one);
   function Create (Mode : Dword_Kind_Mode_Type) return DWord;

   --further tools needed------------------------------------------------------

   --removes delimiters from given String
    --delimiters are ' ' and '.' and '_'
    --also makes the input lowercase
   procedure remove_Delimiters(text : in out Unbounded_String);

   function get_Value_From_Hex_Char(Hex_Char : Character) return Natural;
   function get_Value_From_Bin_Char(Bin_Char : Character) return Natural;


end Crypto.Symmetric.Algorithm.Threefish;
