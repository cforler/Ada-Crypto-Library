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

with Ada.Unchecked_Conversion;
with Ada.Containers.Doubly_Linked_Lists;
with Crypto.Types; use Crypto.Types;


package Crypto.Types.Skein is
    --the Mode type, just for distinguishing the diffrent mode of Skein
    type Skein_Mode is (m256, m512, m1024);
    function Get_Number_Of_Skein_Bytes(Mode : in Skein_Mode)
            return Natural;


    type Bytes_Access is access Bytes;

    --a group of Bytes we want to work on using different tasks
    --we need this for UBI-Tree-Mode (we are calculating different parts
    --of the inner state using different tasks)
    protected type Protected_Bytes(Last_Index : Natural) is
        --sets a number of Bytes in the group of Bytes
        procedure Set_Bytes(In_Bytes     : in Bytes;
                            Index_First  : in Natural;
                            Index_Last   : in Natural);
        --returns the whole group of Bytes
        procedure Return_Bytes(Out_Bytes : out Bytes);
    private
        B : Bytes(0..Last_Index);
    end Protected_Bytes;

    --if we have lengths of Bits mod 8 != 0 then we can use this
    --function to get the correct size for an Byte-Array
    function Create(Message_Length_Bits : Natural) return Bytes;

    --if we want something like toString we can use this functions
    function Show_Bin(b : Byte) return String;
    function Show_Hex(b : Byte) return String;

    --sometimes we need to set a single Bit inside of a Byte
    procedure Set_Bit(b        : in out Byte;
                      Position : in     Natural;
                      Value    : in     Boolean);

    --in Threefish itself we use Words of defined length
    --of 64 Bit
    --we need some functions to work on this special type
    --and some depending types such as arays of them
    type Skeinword is mod 2**64;
    type Boolean_Array_64 is array(0..63) of Boolean;
    for Boolean_Array_64'Component_Size use 1;
    function Skeinword_To_Boolean_Array_64 is
        new Ada.Unchecked_Conversion (Source => Skeinword,
                                      Target => Boolean_Array_64);

    --some Array Types of Skeinwords
    --we need them for the internals in Threefish
    type Skeinword_Array is array (Natural range <>) of Skeinword;
    type Skeinword_Matrix is array (Natural range <>,Natural range <>) of skeinword;

    --some "toString" functions
    function Show_Bin(sw1 : Skeinword) return String;
    function Show_Hex(sw1 : Skeinword) return String;

    --some simple Create functions
    --inputs can be Natural
    --or Hex Stings (=Array of Chars)
    --or Binary String (=Array of Chars)
    --or random/allOne/allZero
    function Create(Input : Natural) return Skeinword;

    --type for supported skeinword-inputs for Create-function with String input
    type Skeinword_Input_Mode_Type is (Hex, Bin);
    function  Create(Input : in String;
                     Mode  : in Skeinword_Input_Mode_Type := Hex) return Skeinword;

    --we also want to be able to create defined skeinwords
    type Skeinword_Kind_Mode_Type is (random, all_zero, all_one);
    function  Create(Mode : Skeinword_Kind_Mode_Type) return Skeinword;




    --we want to have some function overwritten for tests
--    function "+" (Left,Right : Skeinword) return Skeinword;
--    function "xor" (Left,Right : Skeinword) return Skeinword;




--    function "*"(Left : Byte;
--                 Right : Integer) return Integer;
    function "+"(Left : Skeinword;
            Right : Integer) return Skeinword;

    procedure Set_Bit(Word     : in out Skeinword;
                     Position : in Natural;
                     Value    : in Boolean);

    function left_rot(sw1   : in Skeinword;
                      count : in Natural) return Skeinword;

    function Natural_To_Bytes(N      : Natural;
                              number : Natural) return Bytes;

    function Bytes_To_Skeinword(b : in Bytes)
            return Skeinword;

    function Bytes_To_Skeinword_Array(b : in Bytes)
            return Skeinword_Array;

    function Skeinword_To_Bytes(s: in Skeinword)
            return Bytes;

    function Skeinword_Array_To_Bytes(s : in Skeinword_Array)
            return Bytes;

    function "+"(Left: Bytes;
                 Right: Natural) return Bytes;


    --a special type for the length of Skein Input Message lengths (in Bytes)
    --Attention, this should be 2**96 but this is not supportet atm
    --we need to get this rigth!!!
    type Skein_Message_Length is mod 2**64;

    --also Input-Messages of length mod 8 != 0 are allowed, so we need
    --a special type for this.
    type Skein_Message_Length_Bits is range 0..Natural'Last*8;

    --for the Full Skein implementation we need a type
    --for storing a list of message and tweak tuples
    --we also want to store the correct message-length in Bits
    --a record type is used for this
    type Skein_Message_Tweak_Tuple(
        Message_Length_Bits : Natural;
        Message_Bytes_Last  : Integer) --we need Integer here because if we have an empty
                                        --array of Byte we need it with index 0..-1
        is record
        Message         : Bytes(0..Message_Bytes_Last);
        Message_Length  : Natural := Message_Length_Bits;
        Type_Value      : Byte;
    end record;
    --we need an array of this record type,
    --as it is an unconstrained type which is not allowed in records
    --we define an seperate acces type and build an array of
    --access types :/
    type Skein_Message_Tweak_Tuple_Pointer is access all Skein_Message_Tweak_Tuple;
    type Skein_Message_Tweak_Tuple_Pointer_Array is array(Natural Range <>)
        of Skein_Message_Tweak_Tuple_Pointer;

    function Create(Size : Natural) return Skein_Message_Tweak_Tuple_Pointer_Array;
    procedure Set_Data(
                List               : in out Skein_Message_Tweak_Tuple_Pointer_Array;
                Index              : in     Natural;
                Message            : in     Bytes;
                Message_Length_Bits: in     Natural;
                Type_Value         : in     Byte);


--    package Skein_Message_Tweak_Tuple_Pointer_List is new Ada.Containers.Doubly_Linked_Lists(
--        Element_Type => Skein_Message_Tweak_Tuple_Pointer);

    --we need a protected type for the tree mode
    --this type counts (indirectly) the number of runs
    protected type Skein_Tree_Message_Length_Counter is
        procedure Set_Final_Length(Value : Natural);
        procedure Reset;
        procedure Increase(Value : Natural);
        entry Is_Final_Length_Reached(Answer: out Boolean);
    private
        Length : Natural := 0;
        Final_Length : Natural := 0;
    end Skein_Tree_Message_Length_Counter;

    --we need an access type of this to use is inside of tasks :/
    type Skein_Tree_Message_Length_Counter_Access is access Skein_Tree_Message_Length_Counter;



    --since there are no tasks with discrimiants possible we need a workaround
    type Skein_Tree_Task_Data is
        record
            Mode                  : Skein_Mode;
            Longest_Message_Bytes : Natural;
        end record;

    function Get_Max(a,b : Natural) return Natural;


    --we need Matrix or tensors of Integes to store various things
    --we need this to initialize a tensor for saving the number of Differences
    --for every word after every round
    --we also use this type for saving the number of differences for every bit
    type Integer_Tensor is array(Natural range <>,
                                Natural range <>,
                                Natural range <>)
        of Integer;

    type Integer_Matrix is Array(Natural Range <>,
                                Natural Range <>)
        of Integer;

    type Integer_Array is array(Natural Range<>)
        of Integer;

end Crypto.Types.Skein;
