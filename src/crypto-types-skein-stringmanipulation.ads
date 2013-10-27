------------------------------------------------------------------------
--
-- Functions and procedures for changing Strings
-- needed for various Skien-create-functions
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------

with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;


--package for stringmanipulations
--containing some procedures and functions we
--need regulary
package Crypto.Types.Skein.Stringmanipulation is

    --converts String to Unbounded_String
    function str2ustr (input : in String) return Ada.Strings.Unbounded.Unbounded_String;

    --converts Unbounded_String to String
    function ustr2str (input : in Ada.Strings.Unbounded.Unbounded_String) return String;


    function starts_With (text, word : in String) return Boolean;

    --removes delimiters from given String
    --delimiters are ' ' and '.' and '_'
    --also makes the input lowercase
    procedure remove_Delimiters(text : in out Unbounded_String);

    --returns a String-representation of Integers without leading space
    function My_Integer_Image (I : Integer) return String;

    function My_Float_Image(F   :   Float;
                            Aft : Natural := 5) return String;

    function get_Value_From_Hex_Char(Hex_Char : Character) return Natural;
    function get_Value_From_Bin_Char(Bin_Char : Character) return Natural;
    function Get_Hex_Char_From_Bin_String(Bin_String : String) return Character;

end Crypto.Types.Skein.Stringmanipulation;
