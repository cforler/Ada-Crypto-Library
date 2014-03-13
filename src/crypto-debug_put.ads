with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
generic
   b : in Boolean := true;
package Crypto.Debug_Put is
   procedure Put_Line(S : in String);
   procedure Put(S : in String);
   procedure Put(Item  : Integer;
      Width : Field := Ada.Integer_Text_IO.Default_Width;
                 Base  : Number_Base := Default_Base);
   procedure New_Line;
end Crypto.Debug_Put;
