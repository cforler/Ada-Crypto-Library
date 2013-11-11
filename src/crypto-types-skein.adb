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

    ------------------------------------------------------------------
    -- we need some functions to Create Dwords from various inputs
    ------------------------------------------------------------------
    function Create(Input : Natural) return Dword is
        My_Dword : Dword := Dword(Input);
    begin
        return My_Dword;
    end Create;

    function  Create(input : in String;
                     Mode  : in Dword_Input_Mode_Type := Hex) return Dword is
        use stringmanipulation;

        output: Dword := 0;
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
                                Dword(get_Value_From_Hex_Char(Full_Hex_String(i))) * 16**(17-i-1) ;
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


    --we need this for the random Dwords
    subtype Random_Type is Integer range 0 .. 15;
    package Random_Pack is new Ada.Numerics.Discrete_Random (Random_Type);
    G : Random_Pack.Generator;

    function Generate_Number return Integer is
    begin
        --Random_Pack.Reset (G);
        return Random_Pack.Random (G);
    end Generate_Number;

    function  Create(Mode : Dword_Kind_Mode_Type) return Dword is
        result : Dword;
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
    procedure Set_Bit(Word     : in out Dword;
                      Position : in Natural;
                      Value    : in Boolean) is
    begin
        if Value then  --we want to set it to true
            Word := Word or Dword(2)**Position;
        else
            Word := Word and (Dword'Last - Dword(2)**Position);
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
    function left_rot(sw1   : in Dword;
                      count : in Natural) return Dword is
    begin
        return Dword(Interfaces.Rotate_Left(Interfaces.Unsigned_64(sw1), count));
    end left_rot;

    --we want to be able to multiply Integers and Bytes
    --we need this in Bytes_To_int for example
--    function "*"(Left : Byte;
--            Right : Integer) return Integer is
--    begin
--        return Integer(Left) * Right;
--    end "*";


--    function "+" (Left,Right : Dword) return Dword is
--    begin
--        return Left xor Right;
--    end "+" ;
--    function "xor" (Left,Right : Dword) return Dword is
--    begin
--        return Left + Right;
--    end "xor" ;






    function "+"(Left : Dword;
                 Right : Integer) return Dword is
    begin
        return Left +  Dword(Right);
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

    function Bytes_To_Dword(b : in Bytes)
            return Dword is
        My_SW : Dword := Dword(0);
    begin
        if not (b'Length = 8) then
            Put_Error_Line("The Length of Bytes must be 8 for converting to Dword");
            Raise Program_Error;
        end if;
        --8 Bytes are one word,
        for j in b'Range loop
            --we need mod 8 here because in call from Bytes_To_Dword_Array
            --the indices are kept :/
            My_SW := My_SW + ( Dword(b(j)) * 256**(j mod 8) );
        end loop;
        return My_SW;
    end Bytes_To_Dword;

    --8 bytes are one Dword
    function Bytes_To_Dword_array(b: in Bytes)
            return Dword_Array is
        My_SW_Array  : Dword_Array(0..b'Length/8 -1);
    begin
        if not (b'Length mod 8 = 0) then
            Put_Error_Line("The Length of Bytes must be a multiple of 8");
            Put_Error_Line(Integer'Image(b'Length));
            Raise Program_Error;
        end if;
        for i in My_SW_Array'Range loop
            My_SW_Array(i) := Bytes_To_Dword(b(8*i..8*i+7));
        end loop;
        return My_SW_Array;
    end Bytes_To_Dword_Array;

    --convert one single Dword to an array of 8 Bytes
    function Dword_To_Bytes(s: in Dword)
            return Bytes is
        My_Bytes_Array : Bytes(0..7);
    begin
        for i in My_Bytes_Array'Range loop
            My_Bytes_Array(i) := Byte( s/256**i mod 256 );
        end loop;
        return My_Bytes_Array;
    end Dword_To_Bytes;

    --one Dword is 8 Bytes
    function Dword_Array_To_Bytes(s : in Dword_array)
            return Bytes is
        My_Bytes : Bytes(0..s'Length*8-1);
    begin
        for i in s'Range loop
            My_Bytes(i*8..i*8+7) := Dword_To_Bytes(s(i));
        end loop;
        return My_Bytes;
    end Dword_Array_To_Bytes;

    function "+"(Left: Bytes;
            Right: Natural) return Bytes is

    begin
        if not (Left'Length = 8) then
            Put_Error_Line("maximum of 8 Byte is allowed for Addition");
            Raise Program_Error;
        end if;
        return Dword_To_Bytes(Bytes_To_Dword(Left) + Right);
    end "+";


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
