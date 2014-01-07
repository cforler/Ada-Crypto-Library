with Ada.Text_IO;

package body Crypto.Debug is

   procedure Put (S : String) is
   begin
      Ada.Text_IO.Put(S);
   end Put;

   procedure Put_Line (S : String) is
   begin
      Ada.Text_IO.Put_Line(S);
   end Put_Line;

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end;



end Crypto.Debug;
