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

with Crypto.Types;
with Crypto.Symmetric.Algorithm.Threefish;
use  Crypto.Symmetric.Algorithm.Threefish;

package Crypto.Symmetric.Algorithm.Skein is

   --Interface fixed to Skein 512 mode, 256 and 1024 mode available as well

   type Generic_Interface is Interface;
   type Skein_512_Context is new Generic_Interface with
      record
         Hash_Value : W_Block512;
      end record;

   ---------------------------------------------------------------------------

   -- low level API with object
   procedure Init(This 		: in out Skein_512_Context);

   procedure Round(This 	: in out 	Skein_512_Context;
                   Message_Block: in 		W_Block512);

   function Final_Round(This 		    : in out Skein_512_Context;
                        Last_Message_Block  : W_Block512;
                        Last_Message_Length : Natural)
                        return W_Block512;

   ---------------------------------------------------------------------------

   -- high level API

   procedure Hash(Message : in String; Hash_Value : out W_Block512);

   procedure Hash(Message : in Bytes;  Hash_Value : out W_Block512);

   procedure F_Hash(Filename : in String; Hash_Value : out W_Block512);

-------------------------------------------------------------------------

   --simplified all-in-one-call for Skein
   --no key or treehashing is used here
   procedure Hash
     (Mode           : in Skein_Mode;
      N_0            : in Natural;
      Message        : in Bytes;
      Message_Length : in Natural;
      Result         : out Bytes);

private

    --sometimes we need to set a single Bit inside of a Byte
   procedure Set_Bit
     (b        : in out Byte;
      Position : in Natural;
      Value    : in Boolean);

   --a special type for the length of Skein Input Message lengths (in Bytes)
   --Attention, this should be 2**96 but this is not supportet atm
   --we need to get this rigth!!!
   type Skein_Message_Length is mod 2 ** 64;

   --also Input-Messages of length mod 8 != 0 are allowed, so we need
   --a special type for this.
   type Skein_Message_Length_Bits is range 0 .. Natural'Last * 8;

   --for the Full Skein implementation we need a type
   --for storing a list of message and tweak tuples
   --we also want to store the correct message-length in Bits
   --a record type is used for this
   type Skein_Message_Tweak_Tuple
     (Message_Length_Bits : Natural;
      Message_Bytes_Last  : Integer)   --we need Integer here because if we
                                       --have an empty
   --array of Byte we need it with index 0..-1
   is
      record
         Message        : Bytes (0 .. Message_Bytes_Last);
         Message_Length : Natural := Message_Length_Bits;
         Type_Value     : Byte;
      end record;

   --we need an array of this record type,
   --as it is an unconstrained type which is not allowed in records
   --we define an seperate acces type and build an array of
   --access types :/
   type Skein_Message_Tweak_Tuple_Pointer is access all
     Skein_Message_Tweak_Tuple;
   type Skein_Message_Tweak_Tuple_Pointer_Array is
     array (Natural range <>) of Skein_Message_Tweak_Tuple_Pointer;

   function Create
     (Size : Natural)
      return Skein_Message_Tweak_Tuple_Pointer_Array;
   procedure Set_Data
     (List                : in out Skein_Message_Tweak_Tuple_Pointer_Array;
      Index               : in Natural;
      Message             : in Bytes;
      Message_Length_Bits : in Natural;
      Type_Value          : in Byte);

   --    package Skein_Message_Tweak_Tuple_Pointer_List is new
   --Ada.Containers.Doubly_Linked_Lists(
   --        Element_Type => Skein_Message_Tweak_Tuple_Pointer);

   --we need a protected type for the tree mode
   --this type counts (indirectly) the number of runs
   protected type Skein_Tree_Message_Length_Counter is
      procedure Set_Final_Length (Value : Natural);
      procedure Reset;
      procedure Increase (Value : Natural);
      entry Is_Final_Length_Reached (Answer : out Boolean);
   private
      Length       : Natural := 0;
      Final_Length : Natural := 0;
   end Skein_Tree_Message_Length_Counter;

   --we need an access type of this to use is inside of tasks :/
   type Skein_Tree_Message_Length_Counter_Access is access
     Skein_Tree_Message_Length_Counter;

   --we need Matrix or tensors of Integes to store various things
   --we need this to initialize a tensor for saving the number of Differences
   --for every word after every round
   --we also use this type for saving the number of differences for every bit
   type Integer_Tensor is
     array (Natural range <>, Natural range <>, Natural range <>) of Integer;

   type Integer_Matrix is
     array (Natural range <>, Natural range <>) of Integer;

   type Integer_Array is array (Natural range <>) of Integer;

   type Bytes_Access is access Bytes;



     --calculates a new Byte-Aray for a given length of Bits
   --the most significant not ued Bit is set to 1
   --all other unused Bits are set to 0
   function Message_Bit_Padding
     (Message        : in Bytes;
      Desired_Length : in Natural)
      return           Bytes;

   --returns a Boolean-value if a Bit padding took place
   --we need this information to calculate the configuration Sting later
   function Get_Bit_Padding_Status
     (Message        : in Bytes;
      Desired_Length : in Natural)
      return           Boolean;

   --padds a given Byte-Array to a given length (defined by the Skein_Mode)
   --too long arays will be cut off to the correct length
   function Message_Byte_Padding
     (Mode             : in Skein_Mode;
      Original_Message : in Bytes)
      return             Bytes;

   --return a modified Tweak for given parameters
   function Get_Current_Tweak
     (T_S       : in Bytes;
      N_M       : in Skein_Message_Length;      --Number of Bytes in Input
                                                --Message
      Index     : in Natural;    --index of Message Block we are curently
                                 --working on
      N_b       : in Natural;    --Number State-Bytes for the current mode
      First_Run : in Boolean;    --a_i
      Last_Run  : in Boolean;    --b_i
      B         : in Boolean)    --was there any BitPadding?
      return      Bytes;

   --calculates the UBI for given parameters
   --see Skein paper for details
   procedure Straight_UBI
     (Mode              : in Skein_Mode;
      G                 : in Bytes;    --starting value on N_B Bytes
      Full_Message      : in Bytes;    --Message of variable lenght
      Full_Message_Bits : in Natural;  --the length of the input Message in
                                       --Bits
      T_S               : in Bytes;    --Starting Tweak T_S of 16 Byte
      Result            : out Bytes);   --the result of UBI:

   task type Tree_UBI_Task
        (Mode                  : Skein_Mode;
         Longest_Message_Bytes : Natural)
      is
      entry compute
        (Mode                : in Skein_Mode;
         G                   : in Bytes;
         Full_Message        : in Bytes;
         Full_Message_Length : in Natural;
         T_S                 : in Bytes;
         Result_Access       : in out Bytes_Access;
         Result_First        : in Natural;
         Result_Last         : in Natural;
         Length_Access       : in out Skein_Tree_Message_Length_Counter_Access)
;
   end Tree_UBI_Task;

   --calculates the UBI in tree-hashing mode
   --see Skein paper for details
   procedure Tree_UBI
     (Mode              : in Skein_Mode;
      G                 : in Bytes;    --starting value on N_B Bytes
      Full_Message      : in Bytes;    --Message of variable lenght
      Full_Message_Bits : in Natural;  --the length of the input Message in
                                       --Bits
      T_S               : in Bytes;    --Starting Tweak T_S of 16 Byte
      Y_l               : in Natural;  --loaf-size for treemode
      Y_f               : in Natural;  --node-size for treemode
      Y_M               : in Natural;  --max tree height
      Result            : out Bytes;    --the result of UBI:
      Number_Of_Tasks   : in Natural := 2);

   --calculates the output of the Skein Hashfunction for a given length
   --internal UBI is used for this
   --see Skein paper for details
   procedure Output
     (Mode   : in Skein_Mode;
      G      : in Bytes;     --the chaining value
      N_0    : in Natural;   --number of required output BITS
      Result : out Bytes);    --the result, if N_0 mod 8 != 0
                              --the last byte is only partially used

   --returns the configuration String C for given parameters
   --this String is used in Init
   function Get_Configuration_String
     (N_0  : in Natural;
      Y_l  : in Natural := 0;
      Y_f  : in Natural := 0;
      Y_m  : in Natural := 0)
      return Bytes;

   --------------------------------------------------------
   -- various Init, Update and Final calls
   --------------------------------------------------------

   --full Init call for Skein
   --State is the result of the Configuration call
   procedure Init
     (Mode  : in Skein_Mode;
      N_0   : in Natural;
      K     : in Bytes;
      Y_l   : in Natural;
      Y_f   : in Natural;
      Y_m   : in Natural;
      State : out Bytes);

   --simplified Init-call for Simple_Skein
   --no key and no tree-hashing is used
   procedure Init
     (Mode  : in Skein_Mode;
      N_0   : in Natural;
      State : out Bytes);

   --full Update call for Skein
   --this Call can be done multiple times for a list of tuples (T,M)
   --New-State is the result after each UBI
   procedure Update
     (Mode           : in Skein_Mode;
      Old_State      : in Bytes;
      Message        : in Bytes;
      Message_Length : in Natural;
      Type_Value     : in Byte;
      Y_l            : in Natural;
      Y_f            : in Natural;
      Y_m            : in Natural;
      New_State      : out Bytes);

   --simplified Update call for Skein
   --no key or treehashing is used
   procedure Update
     (Mode           : in Skein_Mode;
      Old_State      : in Bytes;
      Message        : in Bytes;
      Message_Length : in Natural;
      New_State      : out Bytes);

   --Final call for Skein
   --here only the correct is calculated using UBI
   --see Skein paper for details
   procedure Final
     (Mode      : in Skein_Mode;
      Old_State : in Bytes;
      N_0       : in Natural;
      New_State : out Bytes);



   --all-in-one-call for Skein
   --here an array of tuples (T,M) is used as inputs
   procedure Hash
     (Mode        : in Skein_Mode;
      N_0         : in Natural;
      K           : in Bytes;
      Y_l         : in Natural;
      Y_f         : in Natural;
      Y_m         : in Natural;
      Tuple_Array : in Skein_Message_Tweak_Tuple_Pointer_Array;
      Result      : out Bytes);

   --all-in-one-call for Skein
   --using only one tuple (T,M)
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
      Result         : out Bytes);

end Crypto.Symmetric.Algorithm.Skein;
