------------------------------------------------------------------------
--
-- Implementation of the Threefish Block Cipher function.
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------
with Crypto.Types.Skein;       use Crypto.Types.Skein;
with Crypto.Types; 	       use Crypto.Types;
--
package Crypto.Symmetric.Algorithm.Threefish is

    --the different supported modes of threefish
    type Threefish_Mode is (mode256, mode512, mode1024);

    --conversation function from Skein-Mode to Threefish-mode
    --the modes are only used to get the size of the internal state
    function Skein_Mode_To_Threefish_Mode(SMode : in Skein_Mode)
            return Threefish_Mode;

    --returns the sizes of arrays for Dwords and keys for the differnet modes
    function Get_Last_Dword_Index(mode : in threefish_mode) return Natural;

    --returns the name of the version for a given mode
    function Get_Name(mode : in threefish_mode) return String;

    --returns the number of Bits used for Dwords and keys for a given threefisg-mode
    function Get_Bit_Count(mode : in Threefish_Mode) return Natural;

    --returns the number of total rounds for a given Threefish Mode
    --this is 72 for TF256 and TF-512, 80 for TF-1024
    function Get_Number_Of_Rounds(mode : in Threefish_Mode) return Natural;

    --returns the number of MIX-operations for the given Threefish Mode
    function Get_Number_Of_Mix_Operations(Mode : in Threefish_Mode) return Natural;


    --records holding array of Dwords
    type Threefish_Dwords( Last_Index : Natural)   is tagged private;
    type Threefish_Keys(  Last_Index : Natural)   is tagged private;
    type Threefish_Tweaks(Last_Index : Natural)   is tagged private;
    type Threefish_State( Last_Index : Natural)   is tagged private;
    type Threefish_Extended_Keys(Last_Index : Natural)  is tagged private;

    procedure Set_Threefish_Dword(Dwords : in out Threefish_Dwords'Class;
                                Index  : in     Natural;
                                Dword   : in     Types.Dword);

    procedure Set_Threefish_Key(keys   : in out Threefish_Keys'Class;
                                Index  : in     Natural;
                                Key    : in     Dword);

    procedure Set_Threefish_Tweak(tweaks : in out Threefish_Tweaks'Class;
                                  Index  : in     Natural;
                                  Tweak  : in     Dword);

    function Get_Threefish_Dword(Dwords : in Threefish_Dwords'Class;
                                Index : in Natural) return Dword;

    function Get_Threefish_Key(Keys : in Threefish_Keys'Class;
                               Index : in Natural) return Dword;

    function Get_Threefish_Tweak(Tweaks : in Threefish_Tweaks'Class;
                                 Index : in Natural) return Dword;

    ----------------------------------------------------------------
    --wrapper function fo creating the Dwords, keys and tweak blocks
    ----------------------------------------------------------------
    function Make_Dwords(mode : threefish_mode)  return Threefish_Dwords'Class;
    function Make_Keys(mode : threefish_mode)   return Threefish_Keys'Class;
    function Make_Tweaks(mode : threefish_mode) return Threefish_Tweaks'Class;
    function Make_Extended_Keys(mode : threefish_mode) return Threefish_Extended_Keys'Class;

    function Make_Dwords(mode : Threefish_Mode;
                        SWA  : Dwords)  return Threefish_Dwords'Class;
    function Make_Keys(mode : Threefish_Mode;
                       SWA  : Dwords)   return Threefish_Keys'Class;
    function Make_Tweaks(mode : Threefish_Mode;
                        SWA  : Dwords) return Threefish_Tweaks'Class;

    --type holding the rotation and permutation constants
    type Threefish_Mix_Variables_Type is tagged
        record
            Input1  : Natural;
            Input2  : Natural;
            Rotconst: Natural;
        end record;

    --returns the MIX-function data for a given round and MIX-function
    --data is read from the Constant arays defined at threefish.adb
    function Get_Mix_Variables (Mode            : in Threefish_Mode;
                                Roundnumber     : in Natural;
                                Mix_Fct_Number  : in Natural)
            return Threefish_Mix_Variables_Type;

    -------------------------------------------------
    --the main procedures of the whole blockcipher --
    -------------------------------------------------
    --inputs are arrays of Bytes
    --conversion from Byte-array to Dword is done here
    procedure Encrypt (Mode             : in     Skein_Mode;
                       Block_Cipher_Key : in     Bytes;
                       Tweak            : in     Bytes;
                       Plaintext        : in     Bytes;
                       Result           :    out Bytes);

    --inputs are the specified types
    --this is the "real" procedure for encryption, the other ones are just wrapper
    procedure Encrypt  (Mode        : in     Threefish_Mode;
                        InDwords     : in     Threefish_Dwords'Class;
                        Keys        : in     Threefish_Keys'Class;
                        Tweaks      : in     Threefish_Tweaks'Class;
                        OutDwords    :    out Threefish_Dwords'Class;
                        Talk_Mode   : in     Boolean := false);

    procedure Decrypt  (mode        : in     Threefish_mode;
                        inDwords     : in     Threefish_Dwords'Class;
                        keys        : in     Threefish_keys'Class;
                        tweaks      : in     Threefish_tweaks'Class;
                        outDwords    :    out Threefish_Dwords'Class;
                        Talk_Mode   : in     Boolean := false);

    procedure Key_Schedule ( mode    : in     Threefish_mode;
                            keys    : in     Threefish_keys'Class;
                            Tweaks  : in     Threefish_tweaks'Class;
                            ext_Keys: in out Threefish_Extended_Keys'Class);

    procedure Key_Injection(Dwords   : in out Threefish_Dwords'Class;
                            ks      : in     Threefish_Extended_Keys'Class;
                            r       : in     Natural);

    procedure Reverse_Key_Injection(Dwords   : in out Threefish_Dwords'Class;
                                    ks      : in     Threefish_Extended_Keys'Class;
                                    r       : in     Natural);

    procedure Threefish_mix(sw1     : in out Dword;
                            sw2     : in out Dword;
                            rotConst: in     Natural);

    procedure Threefish_Reverse_mix(sw1     : in out Dword;
                                    sw2     : in out Dword;
                                    rotConst: in     Natural);
    -------------------------------------
    --some helper functions
    -------------------------------------


    --check to see if the input has the correct lenght for the given mode
    --maybe we can throw a exception here later
    function Threefish_Mode_Check(  mode   : Threefish_mode;
                                    keys   : Threefish_keys'Class;
                                    tweaks : Threefish_tweaks'Class)
            return boolean;

    procedure Show_Dwords(Talk_Mode : Boolean;
            message   : String;
            Dwords     : Threefish_Dwords'Class);

private
    type Threefish_Dwords(Last_Index : Natural) is tagged
        record
            data  : Dwords(0..Last_Index);
        end record;

    type Threefish_Keys(Last_Index : Natural) is tagged
        record
            data : Dwords(0..Last_Index);
        end record;

    type Threefish_Tweaks(Last_Index : Natural) is tagged
        record
            data : Dwords(0..1);
        end record;

    type Threefish_State(Last_Index : Natural) is tagged
        record
            Dwords : Crypto.Types.Dwords(0..Last_Index);
            keys  : Crypto.Types.Dwords(0..Last_Index);
            tweaks: Crypto.Types.Dwords(0..2);
        end record;

    type Threefish_Extended_Keys(Last_Index : Natural) is tagged
        record
            --in fact we just need 18 entries for tf-256 and tf-512
            data : Dword_Matrix(0..20,0..Last_Index);
        end record;


end Crypto.Symmetric.Algorithm.Threefish;
