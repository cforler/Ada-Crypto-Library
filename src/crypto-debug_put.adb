package body Crypto.Debug_Put is


   procedure Put_Line(S : in String) is
   begin
      if b then
         Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                              Item => S);
      end if;
   end;


   procedure Put(S : in String) is
   begin
      if b then
         Ada.Text_IO.Put(File => Ada.Text_IO.Standard_Error,
                              Item => S);
      end if;
   end;

   procedure Put(Item  : Integer;
                 Width : Field := Default_Width;
                 Base  : Number_Base := Default_Base) is
   begin
      if b then
         Ada.Integer_Text_IO.Put(Item  => Item,
                                 Width => Width,
                                 Base  => Base);
      end if;
   end Put;

   procedure New_Line is
   begin
      if b then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;



end Crypto.Debug_Put;
