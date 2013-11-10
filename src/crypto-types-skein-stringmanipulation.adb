------------------------------------------------------------------------
--
-- Functions and procedures for changing Strings
-- needed for various Skien-create-functions
--
-- Source code author: Martin Kausche, 2008.
--
-- This algorithm and source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.String_Split;         use GNAT.String_Split;
with Ada.Characters.Handling;   use Ada.Characters.Handling;

package body Crypto.Types.Skein.Stringmanipulation is


    --> converts strings to unbounded strings
    function str2ustr (input : in String) return Ada.Strings.Unbounded.Unbounded_String is
    begin
        return Ada.Strings.Unbounded.To_Unbounded_String (input);
    end str2ustr;

    --> converts unbounded strings to strings
    function ustr2str (input : in Ada.Strings.Unbounded.Unbounded_String) return String is
    begin
        return Ada.Strings.Unbounded.To_String (input);
    end ustr2str;

    function starts_With (text, word : in String) return Boolean is
        set   : Character_Set := To_Set (" ");
        words : Slice_Set;
    begin
        GNAT.String_Split.Create (words, text, set, Multiple);
        return Slice (words, 1) = word;
    end starts_With;

    ------------------
    --removes all delimiters and spaces from a given unbounded String
    --makes everything lowercase
    procedure remove_Delimiters(text : in out Unbounded_String) is
        Last_Index : Natural := ustr2str(text)'Last;
        new_text   : String(1..Last_Index) := ustr2str(text);
        counter    : Natural := 1;
    begin
        for i in 1..Last_Index loop
            if (new_text(i) = ' ' or new_text(i) = '.' or new_text(i) = '_') then
                null;
                --Put_LIne("this was a delimiter");
            else
                new_text(counter) := new_text(i);
                counter := counter +1;
            end if;
        end loop;
        --make it lowercase
        new_Text := To_Lower(new_Text);
        --set the out-parameter
        text := str2ustr(new_text(1..counter-1));
    end remove_Delimiters;

    function My_Integer_Image (I : Integer) return String is
    begin
        return Ada.Strings.Fixed.Trim (Integer'Image (I), Ada.Strings.Left);
    end My_Integer_Image;

    function My_Float_Image(F   :   Float;
                            Aft : Natural := 5) return String is
        s : String(1..Aft+5);
    begin
        Ada.Float_Text_IO.Put(
            To => s,
            Item => F,
            Aft => Aft,
            Exp => 0);

        return s;

    end My_Float_Image;

    function get_Value_From_Hex_Char(Hex_Char : Character) return Natural is
    begin
        case Hex_Char is
            when '0' => return 0;
            when '1' => return 1;
            when '2' => return 2;
            when '3' => return 3;
            when '4' => return 4;
            when '5' => return 5;
            when '6' => return 6;
            when '7' => return 7;
            when '8' => return 8;
            when '9' => return 9;
            when 'a' => return 10;
            when 'b' => return 11;
            when 'c' => return 12;
            when 'd' => return 13;
            when 'e' => return 14;
            when 'f' => return 15;
            when others =>
                --TODO: set all zero, abort whole operation, raise exception
                Put_Line("incorrect input for hex, please check");
                raise Program_Error;
        end case;
    end get_Value_From_Hex_Char;

    function get_Value_From_Bin_Char(Bin_Char : Character) return Natural is
    begin
        case Bin_Char is
            when '0' => return 0;
            when '1' => return 1;
            when others =>
                --TODO: set all zero, abort whole operation, raise exception
                Put_Line("incorrect input for bin, please check");
                raise Program_Error;
        end case;
    end get_Value_From_Bin_Char;

end Crypto.Types.Skein.Stringmanipulation;
