with Ada.Text_IO; use Ada.Text_IO; 

package body Crypto.Random_Source is
   
   procedure Read(This : in Random_Source'Class; Byte_Array : out Bytes) is
   begin
      Put("FOO");
      for I in Byte_Array'Range loop
	 This.Read(Byte_Array(I));
      end loop;
   end Read;
   
end Crypto.Random_Source;
