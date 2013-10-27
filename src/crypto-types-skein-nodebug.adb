-- Author: martin
with Ada.Text_IO;
package body Crypto.Types.Skein.Nodebug is
    procedure Put(input      : in String) is
    begin
        null;
    end Put;
    procedure Put_Line(input : in String) is
    begin
        null;
    end Put_Line;

    procedure Put(input              : in Integer;
                  Leading_Spaces     : in Boolean := true) is
    begin
        null;
    end Put;
    procedure Put_Line(input         : in Integer;
                      Leading_Spaces : in Boolean := true) is
    begin
        null;
    end Put_Line;

    procedure Put_Error_Line(input : in String) is
    begin
        --even in nodebug-Mode we want Error Output
        --maybe we get this red :)
        Ada.Text_IO.Put_Line(input);
    end Put_Error_Line;
begin
    null;
end Crypto.Types.Skein.Nodebug;
