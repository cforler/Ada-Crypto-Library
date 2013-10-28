------------------------------------------------------------------------
--
-- Implementation of the Skein hash function.
-- Definition of types
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------

--with skein_debug;               use skein_debug;
with Crypto.Types.Skein.Nodebug; use Crypto.Types.Skein.Nodebug;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Interfaces;
with Crypto.Types.Skein.Stringmanipulation;        use Crypto.Types.Skein.Stringmanipulation;

package body Crypto.Types.Skein is
    function Get_Number_Of_Bytes(Mode : in Skein_Mode)
            return Natural is
    begin
        case Mode is
            when m256  => return 32;
            when m512  => return 64;
            when m1024 => return 128;
        end case;
    end Get_Number_Of_Bytes;

    function Create(Message_Length_Bits : Natural) return Bytes is
        function Get_Bytes_Last(Bit_Length : Natural) return Natural is
        begin
            if Bit_Length mod 8 = 0 then
                return Bit_Length/8 -1;
            else
                return Bit_Length/8;
            end if;
        end Get_Bytes_Last;
        return_Bytes : Bytes(0..Get_Bytes_Last(Message_Length_Bits))
                     := (others => Byte(0));
    begin
        return return_Bytes;
    end Create;

    protected body Protected_Bytes is
        procedure Set_Bytes(In_Bytes    : in Bytes;
                            Index_First : in Natural;
                            Index_Last  : in Natural) is
            i2 : Natural := 0;
        begin
            --TODO: check correct length!!!
            --B(Index_First..Index_Last) := In_Bytes;   --this would be nicer, but gnatmake yells
            for i in Index_First..Index_Last loop
                B(i) := In_Bytes(i2);
                i2 := i2 +1;
            end loop;
        end Set_Bytes;
        procedure Return_Bytes(Out_Bytes : out Bytes) is
        begin
            Out_Bytes := B;
        end Return_Bytes;
    end Protected_Bytes;
    ------------------------------------------------------------------
    -- we need some functions to Create Skeinwords from various inputs
    ------------------------------------------------------------------
    function Create(Input : Natural) return Skeinword is
        My_Skeinword : Skeinword := Skeinword(Input);
    begin
        return My_Skeinword;
    end Create;

    function  Create(input : in String;
                     Mode  : in Skeinword_Input_Mode_Type := Hex) return Skeinword is

        output: Skeinword := 0;
        Input_Copy  : Unbounded_String := str2ustr(input);

        --needed to fill missing lengths
        Full_Hex_String : String(1..16);
        Full_Bin_String : String(1..64);
    begin

        --first lets remove all delimiters TODO
        remove_Delimiters(Input_Copy);

        case Mode is
            when Hex =>
                --fill missing inputs with zeros / cut too long inputs
                for i in reverse 1..16 loop
                    --Put_Line("i: " & i'Img & "    input'Last: " & input'Last'Img);
                    if i>ustr2str(Input_Copy)'Last then
                        Full_Hex_String(17-i) := '0';
                    else
                        --fill with correct entry from input
                        Full_Hex_String(17-i) := ustr2str(Input_Copy)( ustr2str(Input_Copy)'Last+1-i );
                    end if;
                end loop;
                --Ada.Text_IO.Put_LIne("The full Hex String: " & Full_Hex_String);
                for i in Full_Hex_String'Range loop --attention, Strings are in Range 1..xxx
                    output := output +
                                Skeinword(get_Value_From_Hex_Char(Full_Hex_String(i))) * 16**(17-i-1) ;
                end loop;


            when Bin =>
                --fill missing inputs with zeros / cut too long inputs
                --all Strings start from index 1
                for i in reverse 1..64 loop
                    if i>ustr2str(Input_Copy)'Last then    --if the input is to short
                        Full_Bin_String(65-i) := '0';
                    else
                        --TODO: check if its '1' or '0' and check for spacers

                        Full_Bin_String(65-i) := ustr2str(Input_Copy)(ustr2str(Input_Copy)'Last+1-i);
                    end if;
                end loop;

                --now lets create the word
                --Put_Line("Create was called for binmode");
                for i in reverse Full_Bin_String'Range loop --attention, Strings are in Range 1..xxx
                    output := output +
                                get_Value_From_Bin_Char(Full_Bin_String(i)) * 2**(65-i-1);
                end loop;
        end case;

        return output;
    end Create;


    --we need this for the random Skeinwords
    subtype Random_Type is Integer range 0 .. 15;
    package Random_Pack is new Ada.Numerics.Discrete_Random (Random_Type);
    G : Random_Pack.Generator;

    function Generate_Number return Integer is
    begin
        --Random_Pack.Reset (G);
        return Random_Pack.Random (G);
    end Generate_Number;

    function  Create(Mode : Skeinword_Kind_Mode_Type) return Skeinword is
        result : Skeinword;
        Result_Hex_String : String := "0000000000000000";
        Hex_Array : array (0..15) of Character
            := ('0','1','2','3','4','5','6','7','8','9',
                'A','B','C','D','E','F');
    begin
        case Mode is
            when random =>
                --get a random number between 0 and 15
                --rand.reset(my_Generator);
                for i in Result_Hex_String'Range loop
                    Result_Hex_String(i) := Hex_Array(Generate_Number);
                end loop;

                --Ada.Text_IO.Put_Line("");
                --Ada.Text_IO.Put_Line(Result_Hex_String);
                result := Create(Result_Hex_String, Hex);
            when all_zero =>
                result := Create("00000000"  , Hex);
            when all_one =>
                result := Create("FFFFFFFF_FFFFFFFF", Hex);
            when others =>
                Put_Error_Line("This should never happen");
                result := Create("000000", Hex);
        end case;
        return result;
    end Create;

    -----------------------------------------------
    -- we want to be able to set a single Bit
    -----------------------------------------------
    procedure Set_Bit(Word     : in out Skeinword;
                      Position : in Natural;
                      Value    : in Boolean) is
    begin
        if Value then  --we want to set it to true
            Word := Word or Skeinword(2)**Position;
        else
            Word := Word and (Skeinword'Last - Skeinword(2)**Position);
        end if;
    end Set_Bit;

    procedure Set_Bit(b        : in out Byte;
                      Position : in     Natural;
                      Value    : in     Boolean) is
    begin
        if Value then
            b := b or Byte(2**Position);
        else
            b := b and Byte'Last - Byte(2**Position);
        end if;
    end Set_Bit;

    -------------------------------------------------
    -- we also need rotation
    -------------------------------------------------
    function left_rot(sw1   : in Skeinword;
                      count : in Natural) return Skeinword is
    begin
        return Skeinword(Interfaces.Rotate_Left(Interfaces.Unsigned_64(sw1), count));
    end left_rot;

    --we want to be able to multiply Integers and Bytes
    --we need this ins_To_int for example
--    function "*"(Left : Byte;
--            Right : Integer) return Integer is
--    begin
--        return Integer(Left) * Right;
--    end "*";


--    function "+" (Left,Right : Skeinword) return Skeinword is
--    begin
--        return Left xor Right;
--    end "+" ;
--    function "xor" (Left,Right : Skeinword) return Skeinword is
--    begin
--        return Left + Right;
--    end "xor" ;






    function "+"(Left : Skeinword;
                 Right : Integer) return Skeinword is
    begin
        return Left +  Skeinword(Right);
    end "+";

    function Natural_To_Bytes(N      : Natural;
                              number : Natural) return Bytes is
        result : Bytes(0..number-1) := (others => Byte(0));
    begin
        for i in result'Range loop  --Natural can be at least 256**3
            --result(i) := Byte( ( N/(256**i) ) mod 256);
            if i < 4 then
                result(i) := Byte( (N/(256**i)) mod 256 );
                --Ada.Text_IO.Put_Line(Show_Hex(result(i)));
            end if;
        end loop;
        return result;
    end Natural_To_Bytes;

    function Bytes_To_Skeinword(b : in Bytes)
            return Skeinword is
        My_SW : Skeinword := Skeinword(0);
    begin
        if not (b'Length = 8) then
            Put_Error_Line("The Length of Bytes must be 8 for converting to Skeinword");
            Raise Program_Error;
        end if;
        --8 Bytes are one word,
        for j in b'Range loop
            --we need mod 8 here because in call from Bytes_To_Skeinword_Array
            --the indices are kept :/
            My_SW := My_SW + ( Skeinword(b(j)) * 256**(j mod 8) );
        end loop;
        return My_SW;
    end Bytes_To_Skeinword;

    --8 bytes are one Skeinword
    function Bytes_To_Skeinword_Array(b: in Bytes)
            return Skeinword_Array is
        My_SW_Array  : Skeinword_Array(0..b'Length/8 -1);
    begin
        if not (b'Length mod 8 = 0) then
            Put_Error_Line("The Length of Bytes must be a multiple of 8");
            Put_Error_Line(Integer'Image(b'Length));
            Raise Program_Error;
        end if;
        for i in My_SW_Array'Range loop
            My_SW_Array(i) := Bytes_To_Skeinword(b(8*i..8*i+7));
        end loop;
        return My_SW_Array;
    end Bytes_To_Skeinword_Array;

    --convert one single Skeinword to an array of 8 Bytes
    function Skeinword_To_Bytes(s: in Skeinword)
            return Bytes is
        My_Bytes_Array : Bytes(0..7);
    begin
        for i in My_Bytes_Array'Range loop
            My_Bytes_Array(i) := Byte( s/256**i mod 256 );
        end loop;
        return My_Bytes_Array;
    end Skeinword_To_Bytes;

    --one Skeinword is 8 Bytes
    function Skeinword_Array_To_Bytes(s : in Skeinword_Array)
            return Bytes is
        My_Bytes : Bytes(0..s'Length*8-1);
    begin
        for i in s'Range loop
            My_Bytes(i*8..i*8+7) := Skeinword_To_Bytes(s(i));
        end loop;
        return My_Bytes;
    end Skeinword_Array_To_Bytes;

    function "+"(Left: Bytes;
            Right: Natural) return Bytes is

    begin
        if not (Left'Length = 8) then
            Put_Error_Line("maximum of 8 Byte is allowed for Addition");
            Raise Program_Error;
        end if;
        return Skeinword_To_Bytes(Bytes_To_Skeinword(Left) + Right);
    end "+";

    --------------------------------------------
    -- some "toString" functions
    --------------------------------------------
    function Show_Bin(sw1 : Skeinword) return String is
        swc : Skeinword := sw1;
        out_String : String(1..64);
    begin
        --
        for i in out_String'Range loop

            if (swc and (0 or 2**(i-1)) ) = 2**(i-1) then
                out_String(65-i) := '1';
            else
                out_String(65-i) := '0';
            end if;
        end loop;
        return out_String;
    end Show_Bin;

    function Show_Hex(sw1 : Skeinword) return String is
        Bin_String : String := Show_Bin(sw1);
        out_String : String(1..16);
        Start_Index : Natural := 1;
    begin
        --we convert 4 chars of the Bin_String to one Char Hex_String
        for i in out_String'Range loop
            Start_Index := 4*i -3;
            out_String(i) := Get_Hex_Char_From_Bin_String(Bin_String(4*i -3 .. 4*i));
        end loop;
        return out_String;
    end Show_Hex;

    function Show_Bin(b : Byte) return String is
        out_String : String(1..8);
    begin
        for i in out_String'Range loop
            if (b and (0 or 2**(i-1)) ) = 2**(i-1) then
                out_String(9-i) := '1';
            else
                out_String(9-i) := '0';
            end if;
        end loop;
        return out_String;
    end Show_Bin;

    function Show_Hex(b : Byte) return String is
        Bin_String : String := Show_Bin(b);
        out_String : String(1..2);
    begin
        --we convert 4 chars of the Bin_String to one Char Hex_String
        for i in out_String'Range loop
            out_String(i) := Get_Hex_Char_From_Bin_String(Bin_String(4*i -3 .. 4*i));
        end loop;
        return out_String;
    end Show_Hex;


    function Create(Size : Natural)
            return Skein_Message_Tweak_Tuple_Pointer_Array is
        foo : Skein_Message_Tweak_Tuple_Pointer_Array(0..Size-1);
    begin
        return foo;
    end Create;

    procedure Set_Data(
            List               : in out Skein_Message_Tweak_Tuple_Pointer_Array;
            Index              : in     Natural;
            Message            : in     Bytes;
            Message_Length_Bits: in     Natural;
            Type_Value         : in     Byte) is
    begin
        List(Index) := new Skein_Message_Tweak_Tuple(
                            Message_Length_Bits => Message_Length_Bits,
                            Message_Bytes_Last  => Message'Last);
        List(Index).all.Message         := Message;
        List(Index).all.Message_Length  := Message_Length_Bits;
        List(Index).all.Type_Value      := Type_Value;
    end Set_Data;

    protected body Skein_Tree_Message_Length_Counter is
        procedure Set_Final_Length(Value : Natural) is
        begin
            Final_Length := Value;
            Put_Line("Final_Length set to:" & Integer'Image(Final_Length));
        end Set_Final_Length;
        procedure reset is
        begin
            Length := 0;
            Put_Line("Length was resetted");
        end reset;

        procedure increase(Value : Natural) is
        begin
            Length := Length + Value;
            Put_LIne("Length increased to:" & Integer'Image(Length));
        end increase;

        entry Is_Final_Length_Reached(Answer: out Boolean)
                when Length = Final_Length is
        begin
            Answer := true;
        end Is_Final_Length_Reached;
    end Skein_Tree_Message_Length_Counter;


    --we need to get this somewhee else
    function Get_Max(a,b : Natural) return Natural is
    begin
        if a > b then
            return a;
        else
            return b;
        end if;
    end Get_Max;

begin --package initialisation
    Random_Pack.Reset (G);
	null;
end Crypto.Types.Skein;
