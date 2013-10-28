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
                        := (((0,1, 5),  (2,3,56)),
                            ((0,3,36),  (2,1,28)),
                            ((0,1,13),  (2,3,46)),
                            ((0,3,58),  (2,1,44)),
                            ((0,1,26),  (2,3,20)),
                            ((0,3,53),  (2,1,35)),
                            ((0,1,11),  (2,3,42)),
                            ((0,3,59),  (2,1,50)));

    Mix_Constants_512 : constant array (0..7, 0..3, 0..2) of Natural
                        := (((0,1,38),  (2,3,30),   (4,5,50),   (6,7,53)),
                            ((2,1,48),  (4,7,20),   (6,5,43),   (0,3,31)),
                            ((4,1,34),  (6,3,14),   (0,5,15),   (2,7,27)),
                            ((6,1,26),  (0,7,12),   (2,5,58),   (4,3, 7)),
                            ((0,1,33),  (2,3,49),   (4,5, 8),   (6,7,42)),
                            ((2,1,39),  (4,7,27),   (6,5,41),   (0,3,14)),
                            ((4,1,29),  (6,3,26),   (0,5,11),   (2,7, 9)),
                            ((6,1,33),  (0,7,51),   (2,5,39),   (4,3,35)));

    Mix_Constants_1024 : constant array (0..7, 0..7, 0..2) of Natural
                            := ((( 0, 1,55),  ( 2, 3,43),  ( 4, 5,37),  ( 6, 7,40),  ( 8, 9,16),  (10,11,22),  (12,13,38),  (14,15,12)),

                                (( 0, 9,25),  ( 2,13,25),  ( 6,11,46),  ( 4,15,13),  (10, 7,14),  (12, 3,13),  (14, 5,52),  ( 8, 1,57)),

                                (( 0, 7,33),  ( 2, 5, 8),  ( 4, 3,18),  ( 6, 1,57),  (12,15,21),  (14,13,12),  ( 8,11,32),  (10, 9,54)),

                                (( 0,15,34),  ( 2,11,43),  ( 6,13,25),  ( 4, 9,60),  (14, 1,44),  ( 8, 5, 9),  (10, 3,59),  (12, 7,34)),

                                (( 0, 1,28),  ( 2, 3, 7),  ( 4, 5,47),  ( 6, 7,48),  ( 8, 9,51),  (10,11, 9),  (12,13,35),  (14,15,41)),

                                (( 0, 9,17),  ( 2,13, 6),  ( 6,11,18),  ( 4,15,25),  (10, 7,43),  (12, 3,42),  (14, 5,40),  ( 8, 1,15)),

                                (( 0, 7,58),  ( 2, 5, 7),  ( 4, 3,32),  ( 6, 1,45),  (12,15,19),  (14,13,18),  ( 8,11, 2),  (10, 9,56)),

                                (( 0,15,47),  ( 2,11,49),  ( 6,13,27),  ( 4, 9,58),  (14, 1,37),  ( 8, 5,48),  (10, 3,53),  (12, 7,56)));

    function Get_Last_Word_Index(mode : in Threefish_mode) return Natural is
    begin
        case mode is
            when mode256  => return 256/64 - 1;
            when mode512  => return 512/64 - 1;
            when mode1024 => return 1024/64 - 1;
        end case;
    end Get_Last_Word_Index;

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
        return Keys.data'Last = Get_Last_Word_Index(mode);
    end Threefish_Mode_Check;


    procedure encrypt ( Mode             : in     Skein_Mode;
                        Block_Cipher_Key : in     Bytes;
                        Tweak            : in     Bytes;
                        Plaintext        : in     Bytes;
                        Result           :    out Bytes) is
        words    : Threefish_Words'Class
                 := Make_Words( Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                                SWA  => Bytes_To_Skeinword_Array(Plaintext));
        Keys     : Threefish_Keys'Class
                 := Make_Keys(Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                              SWA  => Bytes_To_Skeinword_Array(Block_Cipher_Key));
        tweaks   : Threefish_Tweaks'Class
                 := Make_Tweaks(Mode => Threefish.Skein_Mode_To_Threefish_Mode(Mode),
                                SWA  => Bytes_To_Skeinword_Array(Tweak));
        outwords : Threefish_Words'Class := Make_Words(Skein_Mode_To_Threefish_Mode(Mode));
    begin
        --Show only the inputs
        --Show_Words(true,"inputwords",words);

        encrypt(Mode => Skein_Mode_To_Threefish_Mode(Mode),
            inwords  => words,
            Keys     => Keys,
            tweaks   => tweaks,
            outwords => outwords,
            Talk_Mode=> false);
        Result := Skeinword_Array_To_Bytes(outwords.data);

        --Show_Words(true,"results",outwords);
    end encrypt ;

    procedure encrypt  (mode      : in Threefish_mode;
                        inwords   : in Threefish_words'Class;
                        Keys      : in Threefish_Keys'Class;
                        tweaks    : in Threefish_tweaks'Class;
                        outwords  : out Threefish_words'Class;
                        Talk_Mode : in Boolean := false) is

    --we calculate the Extended Keys here (KeyShedule)
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Word_Index(mode));

    --we have to count the rounds and the keyinjections
        Round_Counter         : Natural := 0;

    --some vaiales needed for the MIX-operations
        TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND
            : constant Natural := Get_Number_Of_Mix_Operations(Mode);
        TOTAL_NUMBER_OF_ROUNDS
            : constant Natural := Get_Number_Of_Rounds(Mode);
        Current_Mix_Variable : Threefish_Mix_Variables_Type;


    begin
        Show_Words(Talk_Mode,"initial Words",inwords);

        if Talk_Mode then
            Put_Line("here are input Keys");
            for i in Keys.data'Range loop
                Put_Line(Show_Hex(Keys.data(i)));
            end loop;
        end if;

        if Talk_Mode then
            Put_Line("here are input tweaks");
            for i in tweaks.data'Range loop
                Put_Line(Show_Hex(tweaks.data(i)));
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
                    Put_line(Show_Hex(Extended_Keys.data(i,j)));
                end loop;
                Put_line("-----------");
            end loop;
        end if;

        -- now we have the Extended Keys
        -- we can do the Keyinjection and the Mixing
        outwords := inwords;

        --the initial keyInjection
        Key_Injection(outwords, Extended_Keys, 0);

        Show_Words(Talk_Mode,"Words after initial keyInjection",outwords);

        --we want to do 72/80 rounds
        --insert the Extended Keys after every 4th round
        --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
        for r in 1..TOTAL_NUMBER_OF_ROUNDS loop
            for j in 1..TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
                --get the values for the current MIX opeation
                Current_Mix_Variable := get_Mix_Variables(mode, r, j);

                --do the mixing
                Threefish_mix(outwords.data(Current_Mix_Variable.input1),
                              outwords.data(Current_Mix_Variable.input2),
                              Current_Mix_Variable.rotconst);
            end loop;

            Show_Words(Talk_Mode,"Words after round: " & r'Img,outwords);

            --do the keyinjection afte every round mod 4 = 0
            if r mod 4 = 0 then
                Key_Injection(  outwords,
                                Extended_Keys,
                                r/4);

                Show_Words(Talk_Mode,"Words after round " & r'Img & " and KeyInjection",outwords);
            end if;



        end loop;

    end encrypt;

    --ATTENTION:
    --we need a new KeyDeInjection
    --we need a new DeMIX
    procedure decrypt  (mode        : in     Threefish_mode;
                        inwords     : in     Threefish_words'Class;
                        Keys        : in     Threefish_Keys'Class;
                        tweaks      : in     Threefish_tweaks'Class;
                        outwords    :    out Threefish_words'Class;
                        Talk_Mode   : in     Boolean := false) is
        --we calculate the Extended Keys here (KeyShedule)
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Word_Index(mode));

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
        outwords := inwords;

        --we want to do 72/80 rounds
        --insert the Extended Keys after every 4th round
        --for i in 0..TOTAL_NUMBER_OF_ROUNDS-1 loop
        for r in reverse 1..TOTAL_NUMBER_OF_ROUNDS loop

            --do the keyinjection after every round mod 4 = 0
            if r mod 4 = 0 then
                Reverse_Key_Injection(  outwords,
                                Extended_Keys,
                                r/4);
            end if;

            for j in 1..TOTAL_NUMBER_OF_MIX_OPERATIONS_PER_ROUND loop
                --get the values for the current MIX opeation
                Current_Mix_Variable := get_Mix_Variables(mode, r, j);

                --do the mixing
                Threefish_Reverse_mix(outwords.data(Current_Mix_Variable.input1),
                              outwords.data(Current_Mix_Variable.input2),
                              Current_Mix_Variable.rotconst);
            end loop;


        end loop;

        --the "initial" keyInjection
        Reverse_Key_Injection(outwords, Extended_Keys, 0);
    end decrypt  ;

    procedure Key_Schedule ( mode    : in     Threefish_mode;
                            Keys    : in     Threefish_Keys'Class;
                            Tweaks  : in     Threefish_tweaks'Class;
                            ext_Keys: in out Threefish_Extended_Keys'Class) is
        --some variables we will need more often
        k_f : Natural := Keys.data'First;
        k_l : Natural := Keys.data'Last;

        --the number of words(just for better readability
        n_w : Natural := k_l + 1;

        --we need longer Keys and tweaks for the calculation
        long_Keys : skeinword_array(0..k_l+1);
        long_tweaks : skeinword_array (0..2);

        --some skeinwords we will need
        c5 : skeinword := create("5555555555555555", Hex);
    begin
        --------------------------
        --first we have fill the longer Keys and tweaks
        long_Keys(0..k_l) := Keys.data(0..k_l);
        long_Keys(n_w) := c5;
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
                Put_Line(Show_Hex(ext_Keys.data(i,0)) & "    "
                    & Show_Hex(ext_Keys.data(i,1))& "    "
                    & Show_Hex(ext_Keys.data(i,2))& "    "
                    & Show_Hex(ext_Keys.data(i,3)));
            end loop;
        end if;

    end Key_Schedule ;


    procedure Key_Injection(words   : in out Threefish_words'Class;
                            ks      : in     Threefish_Extended_Keys'Class;
                            r       : in     Natural) is
    begin
        --we just have to ADD words and Extended Keys for the current round
        for w in words.data'Range loop
            words.data(w) := words.data(w) + ks.data(r,w);
        end loop;
    end Key_Injection;

    procedure Reverse_Key_Injection(words   : in out Threefish_words'Class;
                                    ks      : in     Threefish_Extended_Keys'Class;
                                    r       : in     Natural) is
    begin
        --we just have to ADD words and Extended Keys for the current round
        for w in words.data'Range loop
            words.data(w) := words.data(w) - ks.data(r/8,w);
        end loop;
    end Reverse_Key_Injection;

    procedure Threefish_mix(sw1     : in out skeinword;
                            sw2     : in out skeinword;
                            rotConst: in     Natural) is
    begin
        sw1 := sw1 + sw2;
        sw2 := left_rot(sw2, rotConst);
        sw2 := sw1 xor sw2;
    end Threefish_mix;

    procedure Threefish_Reverse_mix(sw1     : in out skeinword;
                                    sw2     : in out skeinword;
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
    function Make_Words(mode : Threefish_mode)  return Threefish_Words'Class is
        words : Threefish_Words(Last_Index => Get_Last_Word_Index(mode));
    begin
        for i in words.data'Range loop
            words.data(i) := Create("000", Hex);
        end loop;
        return words;
    end make_words;
--------
    function Make_Keys(mode : Threefish_mode)   return Threefish_Keys'Class is
        Keys : Threefish_Keys(Get_Last_Word_Index(mode));
    begin
        for i in Keys.data'Range loop
            Keys.data(i) := Create("000", Hex);
        end loop;
        return Keys;
    end make_Keys;
--------
    function Make_Tweaks(mode : Threefish_mode) return Threefish_Tweaks'Class is
        tweaks : Threefish_Tweaks(Get_Last_Word_Index(mode));
    begin
        for i in tweaks.data'Range loop
            tweaks.data(i) := Create("000", Hex);
        end loop;
        return tweaks;
    end make_tweaks;
---------
    function Make_Extended_Keys(mode : Threefish_mode) return Threefish_Extended_Keys'Class is
        Extended_Keys : Threefish_Extended_Keys(Get_Last_Word_Index(mode));
    begin
        for i in Extended_Keys.data'Range(1) loop
            for j in Extended_Keys.data'Range(2) loop
                Extended_Keys.data(i,j) := Create("000", Hex);
            end loop;
        end loop;
        return Extended_Keys;
    end make_Extended_Keys;
----------
    function Make_Words(mode : Threefish_mode;
            SWA  : Skeinword_Array)  return Threefish_Words'Class is
        words : Threefish_Words(Last_Index => Get_Last_Word_Index(mode));
    begin
        if not (SWA'Last = Get_Last_Word_Index(mode)) then
            Put_Error_Line("Wrong Range, please check");
            Raise Program_Error;
        end if;
        for i in words.data'Range loop
            words.data(i) := SWA(i);
        end loop;
        return words;
    end Make_Words;
---------------
    function Make_Keys(mode : Threefish_Mode;
            SWA  : Skeinword_Array)   return Threefish_Keys'Class is
        Keys : Threefish_Keys(Last_Index => Get_Last_Word_Index(mode));
    begin
        if not (SWA'Last = Get_Last_Word_Index(mode)) then
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
                SWA  : Skeinword_Array) return Threefish_Tweaks'Class is
        tweaks : Threefish_Tweaks(Last_Index => Get_Last_Word_Index(mode));
    begin
        if not (SWA'Last = 1) then
            Put_Error_Line("Wrong Range for Tweaks, please check:"
                & Integer'Image(SWA'Last) & " vs."
                & Integer'Image(Get_Last_Word_Index(mode)));
            Raise Program_Error;
        end if;
        for i in tweaks.data'Range loop
            tweaks.data(i) := SWA(i);
        end loop;
        return tweaks;
    end Make_Tweaks;

    procedure Show_Words(Talk_Mode : Boolean;
                         message   : String;
                         words     : Threefish_Words'Class) is
    begin
        if Talk_Mode then
            Put_Line("-------------------------------");
            Put_Line(message);
            for i in words.data'Range loop
                Put(Show_Hex(words.data(i)));
                Put("    ");
                if (i+1) mod 4 = 0 then
                    Put_Line(" ");
                end if;
                --Put_Line(show(words.data(i)));
            end loop;
        end if;
    end Show_Words;


    procedure Set_Threefish_Word(Words : in out Threefish_Words'Class;
                                Index  : in     Natural;
                                Word   : in     Skeinword) is
    begin
        Words.data(Index) := Word;
    end Set_Threefish_Word;

    procedure Set_Threefish_Key(Keys   : in out Threefish_Keys'Class;
                                Index  : in     Natural;
                                Key    : in     Skeinword) is
    begin
        Keys.data(Index) := Key;
    end Set_Threefish_Key;

    procedure Set_Threefish_Tweak(tweaks : in out Threefish_Tweaks'Class;
                                  Index  : in     Natural;
                                  Tweak  : in     Skeinword) is
    begin
        tweaks.data(Index) := tweak;
    end Set_Threefish_Tweak;

    function Get_Threefish_Word(Words : in Threefish_Words'Class;
                                Index : in Natural) return Skeinword is
    begin
        return Words.data(Index);
    end Get_Threefish_Word;

    function Get_Threefish_Key(Keys : in Threefish_Keys'Class;
                               Index : in Natural) return Skeinword is
    begin
        return Keys.data(Index);
    end Get_Threefish_Key;

    function Get_Threefish_Tweak(Tweaks : in Threefish_Tweaks'Class;
                                 Index : in Natural) return Skeinword is
    begin
        return Tweaks.data(Index);
    end Get_Threefish_Tweak;
end Crypto.Symmetric.Algorithm.Threefish;
