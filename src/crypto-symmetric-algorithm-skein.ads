------------------------------------------------------------------------
--
-- Implementation of the Skein hash function.
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------
with Crypto.Types.Skein;        use Crypto.Types.Skein;

package Crypto.Symmetric.Algorithm.Skein is
    --calculates a new Byte-Aray for a given length of Bits
    --the most significant not ued Bit is set to 1
    --all other unused Bits are set to 0
    function Message_Bit_Padding(
            Message        : in Bytes;
            Desired_Length : in Natural) return Bytes;

    --returns a Boolean-value if a Bit padding took place
    --we need this information to calculate the configuration Sting later
    function Get_Bit_Padding_Status(
            Message        : in Bytes;
            Desired_Length : in Natural) return Boolean;

    --padds a given Byte-Array to a given length (defined by the Skein_Mode)
    --too long arays will be cut off to the correct length
    function Message_Byte_Padding(
            Mode             : in Skein_Mode;
            Original_Message : in Bytes) return Bytes;

    --return a modified Tweak for given parameters
    function Get_Current_Tweak(
            T_S       : in Bytes;
            N_M       : in Skein_Message_Length;    --Number of Bytes in Input Message
            Index     : in Natural;    --index of Message Block we are curently working on
            N_b       : in Natural;    --Number State-Bytes for the current mode
            First_Run : in Boolean;    --a_i
            Last_Run  : in Boolean;    --b_i
            B         : in Boolean)    --was there any BitPadding?
            return Bytes;

    --calculates the UBI for given parameters
    --see Skein paper for details
    procedure Straight_UBI(
            Mode                  : in     Skein_Mode;
            G                     : in     Bytes;    --starting value on N_B Bytes
            Full_Message          : in     Bytes;    --Message of variable lenght
            Full_Message_Bits     : in     Natural;  --the length of the input Message in Bits
            T_S                   : in     Bytes;    --Starting Tweak T_S of 16 Byte
            Result                :    out Bytes);   --the result of UBI:

    task type Tree_UBI_Task(Mode : Skein_Mode;
            Longest_Message_Bytes : Natural)
        is
        entry compute(  Mode                : in     Skein_Mode;
                        G                   : in     Bytes;
                        Full_Message        : in     Bytes;
                        Full_Message_Length : in     Natural;
                        T_S                 : in     Bytes;
                        Result_Access       : in out Bytes_Access;
                        Result_First        : in     Natural;
                        Result_Last         : in     Natural;
                        Length_Access       : in out Skein_Tree_Message_Length_Counter_Access);
    end Tree_UBI_Task;

    --calculates the UBI in tree-hashing mode
    --see Skein paper for details
    procedure Tree_UBI(
            Mode                  : in     Skein_Mode;
            G                     : in     Bytes;    --starting value on N_B Bytes
            Full_Message          : in     Bytes;    --Message of variable lenght
            Full_Message_Bits     : in     Natural;  --the length of the input Message in Bits
            T_S                   : in     Bytes;    --Starting Tweak T_S of 16 Byte
            Y_l                   : in     Natural;  --loaf-size for treemode
            Y_f                   : in     Natural;  --node-size for treemode
            Y_M                   : in     Natural;  --max tree height
            Result                :    out Bytes;    --the result of UBI:
            Number_Of_Tasks       : in     Natural := 2);

    --calculates the output of the Skein Hashfunction for a given length
    --internal UBI is used for this
    --see Skein paper for details
    procedure Output(
            Mode       : in     Skein_Mode;
            G          : in     Bytes;     --the chaining value
            N_0        : in     Natural;   --number of required output BITS
            Result     :    out Bytes);    --the result, if N_0 mod 8 != 0
                                           --the last byte is only partially used

    --returns the configuration String C for given parameters
    --this String is used in Init
    function Get_Configuration_String(
                N_0 : in Natural;
                Y_l : in Natural := 0;
                Y_f : in Natural := 0;
                Y_m : in Natural := 0)
            return Bytes;

    --------------------------------------------------------
    -- various Init, Update and Final calls
    --------------------------------------------------------

    --full Init call for Skein
    --State is the result of the Configuration call
    procedure Init(
            Mode : in     Skein_Mode;
            N_0  : in     Natural;
            K    : in     Bytes ;
            Y_l  : in     Natural;
            Y_f  : in     Natural;
            Y_m  : in     Natural;
            State:    out Bytes);

    --simplified Init-call for Simple_Skein
    --no key and no tree-hashing is used
    procedure Init(
            Mode : in     Skein_Mode;
            N_0  : in     Natural;
            State:    out Bytes);

    --full Update call for Skein
    --this Call can be done multiple times for a list of tuples (T,M)
    --New-State is the result after each UBI
    procedure Update(
            Mode            : in     Skein_Mode;
            Old_State       : in     Bytes;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Type_Value      : in     Byte;
            Y_l             : in     Natural;
            Y_f             : in     Natural;
            Y_m             : in     Natural;
            New_State       :    out Bytes);

    --simplified Update call for Skein
    --no key or treehashing is used
    procedure Update(
            Mode            : in     Skein_Mode;
            Old_State       : in     Bytes;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            New_State       :    out Bytes);

    --Final call for Skein
    --here only the correct is calculated using UBI
    --see Skein paper for details
    procedure Final(
            Mode        : in     Skein_Mode;
            Old_State   : in     Bytes;
            N_0         : in     Natural;
            New_State   :    out Bytes);

    --all-in-one-call for Skein
    --here an array of tuples (T,M) is used as inputs
    procedure Hash(
            Mode        : in     Skein_Mode;
            N_0         : in     Natural;
            K           : in     Bytes;
            Y_l         : in     Natural;
            Y_f         : in     Natural;
            Y_m         : in     Natural;
            Tuple_Array : in     Skein_Message_Tweak_Tuple_Pointer_Array;
            Result      :    out Bytes);

    --all-in-one-call for Skein
    --using only one tuple (T,M)
    procedure Hash(
            Mode            : in     Skein_Mode;
            N_0             : in     Natural;
            K               : in     Bytes;
            Y_l             : in     Natural;
            Y_f             : in     Natural;
            Y_m             : in     Natural;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Type_Value      : in     Byte;
            Result          :    out Bytes);

    --simplified all-in-one-call for Skein
    --no key or treehashing is used here
    procedure Hash(
            Mode            : in     Skein_Mode;
            N_0             : in     Natural;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Result          :    out Bytes);

end Crypto.Symmetric.Algorithm.Skein;
