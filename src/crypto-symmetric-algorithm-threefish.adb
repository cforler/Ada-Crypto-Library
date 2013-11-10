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

with Crypto.Types.Skein.Nodebug;           use Crypto.Types.Skein.Nodebug;
--with skein_nodebug;         use skein_nodebug;
with Crypto.Types.Skein;           use Crypto.Types.Skein;

package body Crypto.Symmetric.Algorithm.Threefish is

    Mix_Constants_256 : constant array (0..7, 0..1, 0..2) of Natural
                        := (((0,1,14),  (2,3,16)),
                            ((0,3,52),  (2,1,57)),
                            ((0,1,23),  (2,3,40)),
                            ((0,3, 5),  (2,1,37)),
                            ((0,1,25),  (2,3,33)),
                            ((0,3,46),  (2,1,12)),
                            ((0,1,58),  (2,3,22)),
                            ((0,3,32),  (2,1,32)));

    Mix_Constants_512 : constant array (0..7, 0..3, 0..2) of Natural
                        := (((0,1,46),  (2,3,36),   (4,5,19),   (6,7,37)),
                            ((2,1,33),  (4,7,27),   (6,5,14),   (0,3,42)),
                            ((4,1,17),  (6,3,49),   (0,5,36),   (2,7,39)),
                            ((6,1,44),  (0,7, 9),   (2,5,54),   (4,3,56)),
                            ((0,1,39),  (2,3,30),   (4,5,34),   (6,7,24)),
                            ((2,1,13),  (4,7,50),   (6,5,10),   (0,3,17)),
                            ((4,1,25),  (6,3,29),   (0,5,39),   (2,7,43)),
                            ((6,1, 8),  (0,7,35),   (2,5,56),   (4,3,22)));

    Mix_Constants_1024 : constant array (0..7, 0..7, 0..2) of Natural
                            := ((( 0, 1,24),  ( 2, 3,13),  ( 4, 5, 8),  ( 6, 7,47),  ( 8, 9, 8),  (10,11,17),  (12,13,22),  (14,15,37)),

                                (( 0, 9,38),  ( 2,13,19),  ( 6,11,10),  ( 4,15,55),  (10, 7,49),  (12, 3,18),  (14, 5,23),  ( 8, 1,52)),

                                (( 0, 7,33),  ( 2, 5, 4),  ( 4, 3,51),  ( 6, 1,13),  (12,15,34),  (14,13,41),  ( 8,11,59),  (10, 9,17)),

                                (( 0,15, 5),  ( 2,11,20),  ( 6,13,48),  ( 4, 9,41),  (14, 1,47),  ( 8, 5,28),  (10, 3,16),  (12, 7,25)),

                                (( 0, 1,41),  ( 2, 3, 9),  ( 4, 5,37),  ( 6, 7,31),  ( 8, 9,12),  (10,11,47),  (12,13,44),  (14,15,30)),

                                (( 0, 9,16),  ( 2,13,34),  ( 6,11,56),  ( 4,15,51),  (10, 7, 4),  (12, 3,53),  (14, 5,42),  ( 8, 1,41)),

                                (( 0, 7,31),  ( 2, 5,44),  ( 4, 3,47),  ( 6, 1,46),  (12,15,19),  (14,13,42),  ( 8,11,44),  (10, 9,25)),

                                (( 0,15, 9),  ( 2,11,48),  ( 6,13,35),  ( 4, 9,52),  (14, 1,23),  ( 8, 5,31),  (10, 3,37),  (12, 7,20)));

    function Get_Last_Dword_Index(mode : in Threefish_mode) return Natural is
    begin
        case mode is
            when mode256  => return 256/64 - 1;
            when mode512  => return 512/64 - 1;
            when mode1024 => return 1024/64 - 1;
        end case;
    end Get_Last_Dword_Index;

    function Get_Name(mode : in Threefish_mode) return String is
    begin
        case mode is
            when mode256  => return "Threefish-256";
            when mode512  => return "Threefish-512";
            when mode1024 => return "Threefish-1024";
        end case;
    end Get_Name;

    function Get_Bit_Count(mode : in Threefish_Mode) return Natural is
    begin
        case mode is
            when mode256  => return 256;
            when mode512  => return 512;
            when mode1024 => return 1024;
        end case;
    end Get_Bit_Count;

    function Get_Number_Of_Rounds(mode : in Threefish_Mode) return Natural is
    begin
        case mode is
            when mode256  => return 72;
            when mode512  => return 72;
            when mode1024 => return 80;
        end case;
    end Get_Number_Of_Rounds;

    function Get_Number_Of_Mix_Operations(Mode : in Threefish_Mode) return Natural is
    begin
        case mode is
            when mode256  => return 2;
            when mode512  => return 4;
            when mode1024 => return 8;
        end case;
    end Get_Number_Of_Mix_Operations;


    function Skein_Mode_To_Threefish_Mode(SMode : in Skein_Mode)
            return Threefish_Mode is
    begin
        case Smode is
            when m256  => return Threefish.mode256;
            when m512  => return Threefish.mode512;
            when m1024 => return Threefish.mode1024;
        end case;
    end Skein_Mode_To_Threefish_Mode;

    function Threefish_Mode_Check(  mode : Threefish_mode;
                                    Keys : Threefish_Keys'Class;
                                    tweaks : Threefish_tweaks'Class)
            return boolean is
    begin
        return Keys.data'Last = Get_Last_Dword_Index(mode);
    end Threefish_Mode_Check;


    procedure encrypt ( Mode             : in     Skein_Mode;
                        Block_Cipher_Key : in     Bytes;
                        Tweak            : in     Bytes;
                        Plaintext        : in     Bytes;
                        Result           :    out Bytes) is
        Dwords    : Threefish_Dwords'Class
                 := Make_Dwords( Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                                SWA  => To_DWords(Plaintext));
        Keys     : Threefish_Keys'Class
                 := Make_Keys(Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                              SWA  => To_DWords(Block_Cipher_Key));
        tweaks   : Threefish_Tweaks'Class
                 := Make_Tweaks(Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                                SWA  => To_DWords(Tweak));
        outDwords : Threefish_Dwords'Class := Make_Dwords(Skein_Mode_To_Threefish_Mode(Mode));
    begin
        --Show only the inputs
        --Show_Dwords(true,"inputDwords",Dwords);

        encrypt(Mode => Skein_Mode_To_Threefish_Mode(Mode),
            inDwords  => Dwords,
            Keys     => Keys,
            tweaks   => tweaks,
            outDwords => outDwords,
            Talk_Mode=> false);
        Result := To_Bytes(outDwords.data);

        --Show_Dwords(true,"results",outDwords);
    end encrypt ;

    procedure encrypt  (mode      : in Threefish_mode;
                        inDwords   : in Threefish_Dwords'Class;
                        Keys      : in Threefish_Keys'Class;
                        tweaks    : in Threefish_tweaks'Class;
                        outDwords  : out Threefish_Dwords'Class;
                        Talk_Mode : in Boolean := false) is

    --we calculate the Extended Keys here (KeyShedule)
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Dword_Index(mode));

    --we have to count the rounds and the keyinjections
        Round_Counter         : Natural := 0;

    --some vaiales needed for the MIX-operations
        TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND
            : constant Natural := Get_Number_Of_Mix_Operations(Mode);
        TOTAL_NUMBER_OF_ROUNDS
            : constant Natural := Get_Number_Of_Rounds(Mode);
        Current_Mix_Variable : Threefish_Mix_Variables_Type;


    begin
        Show_Dwords(Talk_Mode,"initial Dwords",inDwords);

        if Talk_Mode then
            Put_Line("here are input Keys");
            for i in Keys.data'Range loop
                Put_Line(To_Hex(Keys.data(i)));
            end loop;
        end if;

        if Talk_Mode then
            Put_Line("here are input tweaks");
            for i in tweaks.data'Range loop
                Put_Line(To_Hex(tweaks.data(i)));
            end loop;
        end if;


        --do the Keyshedule
        Key_Schedule ( mode, Keys, tweaks, Extended_Keys);

        if Talk_Mode and false then
            --show the expanded Keys
            Put_Line("");
            Put_Line("Here are the expanded Keys");
            for i in Extended_Keys.data'Range loop
                for j in Keys.data'Range loop
                    Put_line(To_Hex(Extended_Keys.data(i,j)));
                end loop;
                Put_line("-----------");
            end loop;
        end if;

        -- now we have the Extended Keys
        -- we can do the Keyinjection and the Mixing
        outDwords := inDwords;

        --the initial keyInjection
        Key_Injection(outDwords, Extended_Keys, 0);

        Show_Dwords(Talk_Mode,"Dwords after initial keyInjection",outDwords);

        --we want to do 72/80 rounds
        --insert the Extended Keys after every 4th round
        --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
        for r in 1..TOTAL_NUMBER_OF_ROUNDS loop
            for j in 1..TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
                --get the values for the current MIX opeation
                Current_Mix_Variable := get_Mix_Variables(mode, r, j);

                --do the mixing
                Threefish_mix(outDwords.data(Current_Mix_Variable.input1),
                              outDwords.data(Current_Mix_Variable.input2),
                              Current_Mix_Variable.rotconst);
            end loop;

            Show_Dwords(Talk_Mode,"Dwords after round: " & r'Img,outDwords);

            --do the keyinjection afte every round mod 4 = 0
            if r mod 4 = 0 then
                Key_Injection(  outDwords,
                                Extended_Keys,
                                r/4);

                Show_Dwords(Talk_Mode,"Dwords after round " & r'Img & " and KeyInjection",outDwords);
            end if;



        end loop;

    end encrypt;

    --ATTENTION:
    --we need a new KeyDeInjection
    --we need a new DeMIX
    procedure decrypt  (mode        : in     Threefish_mode;
                        inDwords     : in     Threefish_Dwords'Class;
                        Keys        : in     Threefish_Keys'Class;
                        tweaks      : in     Threefish_tweaks'Class;
                        outDwords    :    out Threefish_Dwords'Class;
                        Talk_Mode   : in     Boolean := false) is
        --we calculate the Extended Keys here (KeyShedule)
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Dword_Index(mode));

        --some vaiales needed for the MIX-operations
        TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND
            : constant Natural := Get_Number_Of_Mix_Operations(Mode);
        TOTAL_NUMBER_OF_ROUNDS
            : constant Natural := Get_Number_Of_Rounds(Mode);
        Current_Mix_Variable : Threefish_Mix_Variables_Type;
    begin

        --do the Keyshedule
        Key_Schedule ( mode, Keys, tweaks, Extended_Keys);

        -- now we have the Extended Keys
        -- we can do the Keyinjection and the "unMixing"
        outDwords := inDwords;

        --we want to do 72/80 rounds
        --insert the Extended Keys after every 4th round
        --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
        for r in reverse 1..TOTAL_NUMBER_OF_ROUNDS loop

            --do the keyinjection after every round mod 4 = 0
            if r mod 4 = 0 then
                Reverse_Key_Injection(  outDwords,
                                Extended_Keys,
                                r/4);
            end if;

            for j in 1..TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
                --get the values for the current MIX opeation
                Current_Mix_Variable := get_Mix_Variables(mode, r, j);

                --do the mixing
                Threefish_Reverse_mix(outDwords.data(Current_Mix_Variable.input1),
                              outDwords.data(Current_Mix_Variable.input2),
                              Current_Mix_Variable.rotconst);
            end loop;


        end loop;

        --the "initial" keyInjection
        Reverse_Key_Injection(outDwords, Extended_Keys, 0);
    end decrypt  ;

    procedure Key_Schedule ( mode    : in     Threefish_mode;
                            Keys    : in     Threefish_Keys'Class;
                            Tweaks  : in     Threefish_tweaks'Class;
                            ext_Keys: in out Threefish_Extended_Keys'Class) is
        --some variables we will need more often
        k_f : Natural := Keys.data'First;
        k_l : Natural := Keys.data'Last;

        --the number of Dwords(just for better readability
        n_w : Natural := k_l + 1;

        --we need longer Keys and tweaks for the calculation
        long_Keys : Dwords(0..k_l+1);
        long_tweaks : Dwords (0..2);

        --some Dwords we will need
        c240 : Dword := create("1BD11BDAA9FC1A22", Hex);
    begin
        --------------------------
        --first we c5have fill the longer Keys and tweaks
        long_Keys(0..k_l) := Keys.data(0..k_l);
        long_Keys(n_w) := c240;
        for i in k_f..k_l loop
            long_Keys(n_w) := long_Keys(n_w) xor Keys.data(i);
        end loop;

        long_tweaks(0..1) := tweaks.data(0..1);
        long_tweaks(2)    := tweaks.data(0) xor tweaks.data(1);

        --now we can calculate the whole Extended Keys matrix
        for r in 0..Get_Number_Of_Rounds(Mode)/4 loop
            for i in 0..n_w-4 loop
                ext_Keys.data(r,i) := long_Keys((r + i)     mod (n_w +1));
            end loop;

            ext_Keys.data(r,n_w-3) := long_Keys((r + n_w-3) mod (n_w + 1)) + long_tweaks(r mod 3);
            ext_Keys.data(r,n_w-2) := long_Keys((r + n_w-2) mod (n_w + 1)) + long_tweaks((r+1) mod 3);
            ext_Keys.data(r,n_w-1) := long_Keys((r + n_w-1) mod (n_w + 1)) + Create(r);
        end loop;

        if false then
            Put_Line("-------------------------------");
            Put_Line("the Extended Keys: ");
            for i in 0..10 loop
                Put_Line(To_Hex(ext_Keys.data(i,0)) & "    "
                    & To_Hex(ext_Keys.data(i,1))& "    "
                    & To_Hex(ext_Keys.data(i,2))& "    "
                    & To_Hex(ext_Keys.data(i,3)));
            end loop;
        end if;

    end Key_Schedule ;


    procedure Key_Injection(Dwords   : in out Threefish_Dwords'Class;
                            ks      : in     Threefish_Extended_Keys'Class;
                            r       : in     Natural) is
    begin
        --we just have to ADD Dwords and Extended Keys for the current round
        for w in Dwords.data'Range loop
            Dwords.data(w) := Dwords.data(w) + ks.data(r,w);
        end loop;
    end Key_Injection;

    procedure Reverse_Key_Injection(Dwords   : in out Threefish_Dwords'Class;
                                    ks      : in     Threefish_Extended_Keys'Class;
                                    r       : in     Natural) is
    begin
        --we just have to ADD Dwords and Extended Keys for the current round
        for w in Dwords.data'Range loop
            Dwords.data(w) := Dwords.data(w) - ks.data(r/8,w);
        end loop;
    end Reverse_Key_Injection;

    procedure Threefish_mix(sw1     : in out Dword;
                            sw2     : in out Dword;
                            rotConst: in     Natural) is
    begin
        sw1 := sw1 + sw2;
        sw2 := left_rot(sw2, rotConst);
        sw2 := sw1 xor sw2;
    end Threefish_mix;

    procedure Threefish_Reverse_mix(sw1     : in out Dword;
                                    sw2     : in out Dword;
                                    rotConst: in     Natural) is
    begin
        sw2 := sw2 xor sw1;
        sw2 := left_rot(sw2, 64 - rotConst); --in fact we want an right_rot

        sw1 := sw1 - sw2;
    end Threefish_Reverse_mix;

    -------------------------------------------
    --the helper functions
    -------------------------------------------
    function get_Mix_Variables (Mode            : in Threefish_mode;
                                Roundnumber     : in Natural;
                                Mix_Fct_Number  : in Natural)
            return Threefish_Mix_Variables_Type is

        my_Mix_Variable : Threefish_Mix_Variables_Type;
    begin
        case mode is
            when mode256 =>
                --thanks to the ads we have to add 1 to the indexes
                --maybe we change this later, then we only have to do it here
                my_Mix_Variable.input1   := Mix_Constants_256((Roundnumber-1) mod 8, Mix_Fct_Number-1, 0);
                my_Mix_Variable.input2   := Mix_Constants_256((Roundnumber-1) mod 8, Mix_Fct_Number-1, 1);
                my_Mix_Variable.rotconst := Mix_Constants_256((Roundnumber-1) mod 8, Mix_Fct_Number-1, 2);

            when mode512 =>
                my_Mix_Variable.input1   := Mix_Constants_512((Roundnumber-1) mod 8, Mix_Fct_Number-1, 0);
                my_Mix_Variable.input2   := Mix_Constants_512((Roundnumber-1) mod 8, Mix_Fct_Number-1, 1);
                my_Mix_Variable.rotconst := Mix_Constants_512((Roundnumber-1) mod 8, Mix_Fct_Number-1, 2);

            when mode1024 =>
                my_Mix_Variable.input1   := Mix_Constants_1024((Roundnumber-1) mod 8, Mix_Fct_Number-1, 0);
                my_Mix_Variable.input2   := Mix_Constants_1024((Roundnumber-1) mod 8, Mix_Fct_Number-1, 1);
                my_Mix_Variable.rotconst := Mix_Constants_1024((Roundnumber-1) mod 8, Mix_Fct_Number-1, 2);
        end case;

        return my_Mix_Variable;
    end get_Mix_Variables ;

-------
    function Make_Dwords(mode : Threefish_mode)  return Threefish_Dwords'Class is
        Dwords : Threefish_Dwords(Last_Index => Get_Last_Dword_Index(mode));
    begin
        for i in Dwords.data'Range loop
            Dwords.data(i) := Create("000", Hex);
        end loop;
        return Dwords;
    end make_Dwords;
--------
    function Make_Keys(mode : Threefish_mode)   return Threefish_Keys'Class is
        Keys : Threefish_Keys(Get_Last_Dword_Index(mode));
    begin
        for i in Keys.data'Range loop
            Keys.data(i) := Create("000", Hex);
        end loop;
        return Keys;
    end make_Keys;
--------
    function Make_Tweaks(mode : Threefish_mode) return Threefish_Tweaks'Class is
        tweaks : Threefish_Tweaks(Get_Last_Dword_Index(mode));
    begin
        for i in tweaks.data'Range loop
            tweaks.data(i) := Create("000", Hex);
        end loop;
        return tweaks;
    end make_tweaks;
---------
    function Make_Extended_Keys(mode : Threefish_mode) return Threefish_Extended_Keys'Class is
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Dword_Index(mode));
    begin
        for i in Extended_Keys.data'Range(1) loop
            for j in Extended_Keys.data'Range(2) loop
                Extended_Keys.data(i,j) := Create("000", Hex);
            end loop;
        end loop;
        return Extended_Keys;
    end make_Extended_Keys;
----------
    function Make_Dwords(mode : Threefish_mode;
            SWA  : Dwords)  return Threefish_Dwords'Class is
        Dwords : Threefish_Dwords(Last_Index => Get_Last_Dword_Index(mode));
    begin
        if not (SWA'Last = Get_Last_Dword_Index(mode)) then
            Put_Error_Line("Wrong Range, please check");
            Raise Program_Error;
        end if;
        for i in Dwords.data'Range loop
            Dwords.data(i) := SWA(i);
        end loop;
        return Dwords;
    end Make_Dwords;
---------------
    function Make_Keys(mode : Threefish_Mode;
            SWA  : Dwords)   return Threefish_Keys'Class is
        Keys : Threefish_Keys(Last_Index => Get_Last_Dword_Index(mode));
    begin
        if not (SWA'Last = Get_Last_Dword_Index(mode)) then
            Put_Error_Line("Wrong Range, please check");
            Raise Program_Error;
        end if;
        for i in Keys.data'Range loop
            Keys.data(i) := SWA(i);
        end loop;
        return Keys;
    end Make_Keys;
-------------
    function Make_Tweaks(mode : Threefish_mode;
                SWA  : Dwords) return Threefish_Tweaks'Class is
        tweaks : Threefish_Tweaks(Last_Index => Get_Last_Dword_Index(mode));
    begin
        if not (SWA'Last = 1) then
            Put_Error_Line("Wrong Range for Tweaks, please check:"
                & Integer'Image(SWA'Last) & " vs."
                & Integer'Image(Get_Last_Dword_Index(mode)));
            Raise Program_Error;
        end if;
        for i in tweaks.data'Range loop
            tweaks.data(i) := SWA(i);
        end loop;
        return tweaks;
    end Make_Tweaks;

    procedure Show_Dwords(Talk_Mode : Boolean;
                         message   : String;
                         Dwords     : Threefish_Dwords'Class) is
    begin
        if Talk_Mode then
            Put_Line("-------------------------------");
            Put_Line(message);
            for i in Dwords.data'Range loop
                Put(To_Hex(Dwords.data(i)));
                Put("    ");
                if (i+1) mod 4 = 0 then
                    Put_Line(" ");
                end if;
                --Put_Line(show(Dwords.data(i)));
            end loop;
        end if;
    end Show_Dwords;


    procedure Set_Threefish_Dword(Dwords : in out Threefish_Dwords'Class;
                                Index  : in     Natural;
                                Dword   : in     Crypto.Types.Dword) is
    begin
        Dwords.data(Index) := Dword;
    end Set_Threefish_Dword;

    procedure Set_Threefish_Key(Keys   : in out Threefish_Keys'Class;
                                Index  : in     Natural;
                                Key    : in     Dword) is
    begin
        Keys.data(Index) := Key;
    end Set_Threefish_Key;

    procedure Set_Threefish_Tweak(tweaks : in out Threefish_Tweaks'Class;
                                  Index  : in     Natural;
                                  Tweak  : in     Dword) is
    begin
        tweaks.data(Index) := tweak;
    end Set_Threefish_Tweak;

    function Get_Threefish_Dword(Dwords : in Threefish_Dwords'Class;
                                Index : in Natural) return Dword is
    begin
        return Dwords.data(Index);
    end Get_Threefish_Dword;

    function Get_Threefish_Key(Keys : in Threefish_Keys'Class;
                               Index : in Natural) return Dword is
    begin
        return Keys.data(Index);
    end Get_Threefish_Key;

    function Get_Threefish_Tweak(Tweaks : in Threefish_Tweaks'Class;
                                 Index : in Natural) return Dword is
    begin
        return Tweaks.data(Index);
    end Get_Threefish_Tweak;
end Crypto.Symmetric.Algorithm.Threefish;
