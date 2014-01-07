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

package Crypto.Types is

   ---------------------------------------------------------------------------
   ---------------------------TYPES-------------------------------------------
   ---------------------------------------------------------------------------

   -- primary types;

   type Bit is mod 2;
   for Bit'Size use 1;

   type Byte  is mod 2 ** 8;
   for Byte'Size use 8;

   type DByte  is mod 2 ** 16;
   for DByte'Size use 16;

   type Word is mod 2 ** 32;
   for Word'Size use 32;

   type DWord is mod 2 ** 64;
   for DWord'Size use 64;

   type Mod_Type is mod 2**32;
   for Mod_Type'Size use 32;


   --package BIO is new Ada.Text_Io.Modular_IO (Byte);
   --package WIO is new Ada.Text_Io.Modular_IO (Word);
   --package DIO is new Ada.Text_Io.Modular_IO (DWord);

   -- Arrays of primary types
   type Bits   is array (Integer range <>) of Bit;
   type Bytes  is array (Integer range <>) of Byte;
   type Words  is array (Integer range <>) of Word;
   type DWords is array (Integer range <>) of DWord;

   type Mod_Types  is array (Natural range <>) of Mod_type;


   subtype Byte_Word_Range  is Natural range 0..3;
   subtype Byte_DWord_Range is Natural range 0..7;

   subtype Byte_Word  is Bytes (Byte_Word_Range);
   subtype Byte_DWord is Bytes (Byte_DWord_Range);

   -- N :  #bits
   -- byte-blocks (B_BlockN): array of N/8 bytes
   subtype B_Block32_Range  is Natural range 0..3;
   subtype B_Block48_Range  is Natural range 0..5;
   subtype B_Block56_Range  is Natural range 0..6;
   subtype B_Block64_Range  is Natural range 0..7;
   subtype B_Block128_Range is Natural range 0..15;
   subtype B_Block160_Range is Natural range 0..19;
   subtype B_Block192_Range is Natural range 0..23;
   subtype B_Block256_Range is Natural range 0..31;

   type B_Block32  is array(B_Block32_Range)  of Byte;
   type B_Block48  is array(B_Block48_Range)  of Byte;
   type B_Block56  is array(B_Block56_Range)  of Byte;
   type B_Block64  is array(B_Block64_Range)  of Byte;
   type B_Block128 is array(B_Block128_Range) of Byte;
   type B_Block160 is array(B_Block160_Range) of Byte;
   type B_Block192 is array(B_Block192_Range) of Byte;
   type B_Block256 is array(B_Block256_Range) of Byte;


   -- word blocks (W_BlockN): array of N/32 Words
   subtype W_Block128_Range is Natural range 0..3;
   subtype W_Block160_Range is Natural range 0..4;
   subtype W_Block192_Range is Natural range 0..5;
   subtype W_Block256_Range is Natural range 0..7;
   subtype W_Block512_Range is Natural range 0..15;

   type W_Block128 is array(W_Block128_Range) of Word;
   type W_Block160 is array(W_Block160_Range) of Word;
   type W_Block192 is array(W_Block192_Range) of Word;
   type W_Block256 is array(W_Block256_Range) of Word;
   type W_Block512 is array(W_Block512_Range) of Word;


   -- double wordblocks (DW_BlockN): array of N/64 Words
   subtype DW_Block128_Range  is Natural range 0..1;
   subtype DW_Block256_Range  is Natural range 0..3;
   subtype DW_Block384_Range  is Natural range 0..5;
   subtype DW_Block512_Range  is Natural range 0..7;
   subtype DW_Block1024_Range is Natural range 0..15;

   type DW_Block128   is array(DW_Block128_Range) of DWord;
   type DW_Block256   is array(DW_Block256_Range)  of DWord;
   type DW_Block384   is array(DW_Block384_Range)  of DWord;
   type DW_Block512   is array(DW_Block512_Range)  of DWord;
   type DW_Block1024  is array(DW_Block1024_Range) of DWord;


   subtype Hex_Byte_Range   is Natural range 1..2;
   subtype Hex_Word_Range   is Natural range 1..8;
   subtype Hex_DWord_Range  is Natural range 1..16;


   subtype Hex_Byte  is String (Hex_Byte_Range);
   subtype Hex_Word  is String (Hex_Word_Range);
   subtype Hex_DWord is String (Hex_DWord_Range);


   subtype Message_Block_Length256  is Natural range 0 ..  32;
   subtype Message_Block_Length512  is Natural range 0 ..  64;
   subtype Message_Block_Length1024 is Natural range 0 .. 128;


   ---------------------------------------------------------------------------
   ---------------------------FUNCTIONS---------------------------------------
   ---------------------------------------------------------------------------

   function Shift_Left   (Value : Byte; Amount : Natural) return Byte;
   function Shift_Right  (Value : Byte; Amount : Natural) return Byte;
   function Rotate_Left  (Value : Byte; Amount : Natural) return Byte;
   function Rotate_Right (Value : Byte; Amount : Natural) return Byte;

   function Shift_Left   (Value : DByte; Amount : Natural) return DByte;
   function Shift_Right  (Value : DByte; Amount : Natural) return DByte;
   function Rotate_Left  (Value : DByte; Amount : Natural) return DByte;
   function Rotate_Right (Value : DByte; Amount : Natural) return DByte;

   function Shift_Left   (Value : Word; Amount : Natural) return Word;
   function Shift_Right  (Value : Word; Amount : Natural) return Word;
   function Rotate_Left  (Value : Word; Amount : Natural) return Word;
   function Rotate_Right (Value : Word; Amount : Natural) return Word;

   function Shift_Left   (Value : DWord; Amount : Natural) return DWord;
   function Shift_Right  (Value : DWord; Amount : Natural) return DWord;
   function Rotate_Left  (Value : DWord; Amount : Natural) return DWord;
   function Rotate_Right (Value : DWord; Amount : Natural) return DWord;

   function Shift_Left   (Value : Mod_Type; Amount : Natural) return Mod_Type;
   function Shift_Right  (Value : Mod_Type; Amount : Natural) return Mod_Type;
   function Rotate_Left  (Value : Mod_Type; Amount : Natural) return Mod_type;
   function Rotate_Right (Value : Mod_Type; Amount : Natural) return Mod_Type;


   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Left (Value : Bytes; Amount : Natural) return Bytes;
   function Shift_Left (Value : B_Block128 ; Amount : Natural) return B_Block128;
   function Shift_Right (Value : Bytes; Amount : Natural) return Bytes;
   function Shift_Right (Value : B_Block128 ; Amount : Natural) return B_Block128;

   --function Shift_Right (Value : Bytes; Amount : Natural) return Bytes;
   --function Shift_Right (Value : B_Block128 ; Amount : Natural) return B_Block128;

   --Operations for Bytes
   function "xor"(Left, Right : Bytes)        return Bytes;
   function "xor"(Left : Bytes; Right : Byte) return Bytes;
   function "+"(Left : Bytes; Right : Byte)   return Bytes;
   function "+"(Left : Byte; Right  : Bytes)  return Bytes;
   function "and"(Left, Right : Bytes)        return Bytes;


   -- Operations for Words
   function "xor"(Left, Right : Words)       return Words;
   function "+"(Left : Words; Right : Word)  return Words;
   function "+"(Left : Word; Right  : Words) return Words;
   function "+"(Left : Words; Right : Byte)  return Words;


   -- Operations for DWords
   function "xor"(Left, Right : DWords)        return DWords;
   function "+"(Left : DWords; Right : DWord)  return DWords;
   function "+"(Left : DWord;  Right : DWords) return DWords;
   function "+"(Left : DWords; Right : Byte)   return DWords;

   -- Bytes to Word
   function To_Word (A,B,C,D : Byte) return Word;
   function To_Word   (X : Byte_Word) return Word;
   function R_To_Word (X : Byte_Word) return Word; -- reverse
   function To_Words(Byte_Array : Bytes) return Words;

   -- Word to Bytes
   function To_Bytes (X : Word) return Byte_Word;
   function R_To_Bytes (X : Word) return Byte_Word; --reverse
   function To_Bytes(Word_Array : Words) return Bytes;

   --Word = b_0 b_1 b2 b_3
   -- ByteX returns b_n
   function Byte0 (W : Word) return Byte;
   function Byte1 (W : Word) return Byte;
   function Byte2 (W : Word) return Byte;
   function Byte3 (W : Word) return Byte;

   -- Bytes to DWord
   function To_DWord    (X : Byte_DWord) return DWord;
   function R_To_DWord  (X : Byte_DWord) return DWord;
   function To_DWords   (Byte_Array : Bytes) return DWords;

   -- DWord to Bytes
   function To_Bytes   (X : DWord)            return Byte_DWord;
   function R_To_Bytes (X : DWord)            return Byte_DWord;
   function To_Bytes   (DWord_Array : DWords) return Bytes;


   --DWord = b_0 b_1 b2 b_3 b_4 b_5 b_6 b_7
   -- ByteX returns b_n
   function Byte0 (D : DWord) return Byte;
   function Byte1 (D : DWord) return Byte;
   function Byte2 (D : DWord) return Byte;
   function Byte3 (D : DWord) return Byte;
   function Byte4 (D : DWord) return Byte;
   function Byte5 (D : DWord) return Byte;
   function Byte6 (D : DWord) return Byte;
   function Byte7 (D : DWord) return Byte;

   -- To_Word
   function To_Word (A,B,C,D : Character) return Word;

   -- String to Bytes
   function To_Bytes(Message : String) return Bytes;

   -- Bytes to String
   function To_String(ASCII : Bytes) return String;


   -- To_Hex
   function To_Hex(B : Byte)  return Hex_Byte;
   function To_Hex(W : Word)  return Hex_Word;
   function To_Hex(D : DWord) return Hex_DWord;

   -- Is_Zero
   -- returns only true if the "input array"  X = (others => 0)
   function Is_Zero(Byte_Array  : Bytes)   return Boolean;
   function Is_Zero(Word_Array  : Words)   return Boolean;
   function Is_Zero(DWord_Array : DWords) return Boolean;


   -- Byte Blocks To Bytes.
   -- Needed for generic packages to convert a specific byte block.
   function To_Bytes(B : B_Block64)  return Bytes;
   function To_Bytes(B : B_Block128) return Bytes;
   function To_Bytes(B : B_Block192) return Bytes;
   function To_Bytes(B : B_Block256) return Bytes;
   function To_Bytes(W : W_Block160) return Bytes;
   function To_Bytes(W : W_Block256) return Bytes;
   function To_Bytes(W : W_Block512) return Bytes;
   function To_Bytes(D : DW_Block256) return Bytes;
   function To_Bytes(D : DW_Block384) return Bytes;
   function To_Bytes(D : DW_Block512) return Bytes;


   -- Bytes To block of Bytes.
   -- Needed for generic packages to convert a specific byte block.
   function To_B_Block64(B : Bytes) return B_Block64;
   function To_B_Block128(B : Bytes) return B_Block128;
   function To_B_Block192(B : Bytes) return B_Block192;
   function To_B_Block256(B : Bytes) return B_Block256;


   -- Bytes To block of words.
   -- Needed for generic packages to convert a specific byte block.
   function To_W_Block160(B : Bytes) return W_Block160;
   function To_W_Block256(B : Bytes) return W_Block256;
   function To_W_Block512(B : Bytes) return W_Block512;


   -- Bytes To block of double words.
   -- Needed for generic packages to convert a specific byte block.
   function To_DW_Block256(B : Bytes) return DW_Block256;
   function To_DW_Block384(B : Bytes) return DW_Block384;
   function To_DW_Block512(B : Bytes) return DW_Block512;
   function To_DW_Block1024(B : Bytes) return DW_Block1024;

   -- Needed for generic packages to convert a specific byte block.
   function "xor"(Left, Right : B_Block64)    return   B_Block64;
   function "xor"(Left, Right : B_Block128)   return   B_Block128;
   function "xor"(Left, Right : W_Block160)   return   W_Block160;
   function "xor"(Left, Right : W_Block256)   return   W_Block256;
   function "xor"(Left, Right : W_Block512)   return   W_Block512;
   function "xor"(Left, Right : DW_Block512)  return  DW_Block512;
   function "xor"(Left, Right : DW_Block1024) return  DW_Block1024;

   function "+"(Left : B_Block128; Right : Byte) return B_Block128;


   -- Splits  byte array of length n into a left part of length
   -- ceiling(n/2) and a right part of length floor(n/2).
   function Left_Part(Block : in Bytes)  return Bytes;
   function Right_Part(Block : in Bytes) return Bytes;

   -- Nested generic package
   generic
      type T is mod <>;
      type T_A is array (Integer range <>) of T;
      with function Shift_Left  (Value : T; Amount : Natural) return T is <>;
      with function Shift_Right (Value : T; Amount : Natural) return T is <>;

   package Generic_Mod_Aux is
      function "xor"(Left, Right : T_A)      return T_A;
      function "xor"(Left : T_A; Right  : T) return T_A;
      function "xor"(Left : T; Right : T_A)  return T_A;

      function "and"(Left, Right : T_A)      return T_A;

      function "+"(Left : T_A; Right : T) return T_A;
      function "+"(Left : T; Right : T_A) return T_A;

      function Is_Zero(Item : T_A) return Boolean;

      function Left_Part (Block : in T_A) return T_A;
      function Right_Part(Block : in T_A) return T_A;

      function Shift_Left(Value : T_A;  Amount : Natural) return T_A;
      function Shift_Right(Value : T_A;  Amount : Natural) return T_A;
   end Generic_Mod_Aux;



   ---------------------------------------------------------------------------
   -------------------------------PRIVATE-------------------------------------
   ---------------------------------------------------------------------------

private
   pragma Inline (To_B_Block128,To_B_Block192,To_B_Block256);
   pragma Inline ("xor","+");
   pragma Inline (R_To_Bytes, To_Bytes);
   pragma Inline (To_Word, Byte0, Byte1, Byte2, Byte3);
   pragma Inline (Byte4, Byte5, Byte6, Byte7);
   pragma Inline (To_DWord, R_To_DWord);
   pragma Inline (Is_Zero);
   pragma Inline (Left_Part, Right_Part);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

   pragma Optimize(Time);
end Crypto.Types;
