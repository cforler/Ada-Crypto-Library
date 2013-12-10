with Ada; use Ada;
with Crypto.Symmetric.Algorithm.SHA512;
with Ada.Text_IO;
with Ada.Integer_Text_IO;


package body Crypto.Symmetric.KDF_SHA512Crypt is

   procedure Derive(Salt	: in 	salt_type;
                    Password	: in	String;
                    Key		: out	W_Block512) is
      return_block : W_Block512:=(Word(1), others=>Word(0));
   begin
      key:= return_block;
   end Derive;


   procedure Derive(Salt	: in 	String;
                    Password	: in	String) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;

      Salt_Bytes : Bytes(0..Salt'Length-1) := To_Bytes(Salt);
      Password_Bytes : Bytes(0..Password'Length-1) := To_Bytes(Password);
      PW_String_Bytes: Bytes(0..4+Salt_Bytes'Length+Password_Bytes'Length-1):=
        To_Bytes("$") & To_Bytes("6") & To_Bytes("$") & Salt_Bytes & To_Bytes("$") & Password_Bytes;

      Digest_A_Bytes : Bytes(0..127):= (others =>0);
      Digest_A_Hash  : DW_Block512;
      Digest_A_Length: Natural := 0;

      Digest_B_Bytes : Bytes(0..127):= (others =>0);
      Digest_B_Hash  : DW_Block512;
      Digest_B_Length: Natural := 0;

      Big_B_Block : DW_Block1024;
      Fivefundred_Bytes : Bytes(0..511);

      Fat_Bytes : Bytes(0..129) := (others => 1);

   begin
      SHA512.Init(Hash_Value => Digest_A_Hash);

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      SHA512.Init(Hash_Value => Digest_B_Hash);

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Big_B_Block := To_DW_Block1024(B => Digest_B_Bytes);


      Ada.Text_IO.Put_Line("Big B Block");
      for I in Big_B_Block'Range loop
         Ada.Text_IO.Put(To_Hex(Big_B_Block(I)));
      end loop;
      Ada.Text_IO.New_Line;


      Fivefundred_Bytes:= To_Bytes(
      SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
                         Last_Message_Length => Digest_B_Length,
                         Hash_Value          => Digest_B_Hash)
                                   );


   end Derive;


   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	DW_Block512) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;
      Rest_Space : Natural;
      Debug : Boolean := false;
   begin

      Rest_Space := 128 - Digest_Bytes_Length;

      if debug then
         Ada.Text_IO.Put_line("Rest Space: ");
      	 Ada.Integer_Text_IO.Put(Integer(Rest_Space));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_line("Add Length: ");
         Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_line("Bytes to add: ");
            if True then
               for I in Bytes_To_Add'Range loop
                  Ada.Text_IO.Put(To_Hex(Bytes_To_Add(I)));
               end loop;
      	    end if;

         Ada.Text_IO.New_Line;
      end if;



      if Rest_Space > Bytes_To_Add'Length then
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Bytes_To_Add'Length-1)
           := Bytes_To_Add;
         Digest_Bytes_Length := Digest_Bytes_Length+Bytes_To_Add'Length;
      else
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Rest_Space-1)
           := Bytes_To_Add(0..Rest_Space-1);
         SHA512.Round(Message_Block => To_DW_Block1024(B => Digest_Bytes),
                      Hash_Value    => Digest_Hash);
         Digest_Bytes := (others => 0);
         if debug then

            Ada.Text_IO.Put_line("digest_byte range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-Rest_Space-1));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_line("from range");
            Ada.Integer_Text_IO.Put(Integer(Rest_Space));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_line("to range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-1));
            Ada.Text_IO.New_Line;
         end if;


         Digest_Bytes(0..Bytes_To_Add'Length-Rest_Space-1) := Bytes_To_Add(Rest_Space .. Bytes_To_Add'Length-1);
         Digest_Bytes_Length := Bytes_To_Add'Length-Rest_Space;
         if debug then
            Ada.Text_IO.Put("new digest bytes: ");
            for I in Digest_Bytes'Range loop
               Ada.Text_IO.Put(To_Hex(Digest_Bytes(I)));
            end loop;
         end if;

      end if;

   end Add_Bytes;



   function Initialize(Parameter	: in	Natural) return Boolean is
   begin
      return true;
   end;


end Crypto.Symmetric.KDF_SHA512Crypt;
