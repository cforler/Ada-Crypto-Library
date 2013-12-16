with Ada.Text_IO;




package body Crypto.Symmetric.KDF_PBKDF2 is

   procedure Derive(Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	W_Block512) is
      WB : W_Block512 := (16#5a827999#, others=>0);
   begin
      key:= wb;
   end Derive;

   function Initialize(Parameter: in	Natural) return Boolean is
   begin
      Rounds := Parameter;
      return true;
   end;

   procedure Derive(Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
      		    DK_Len	: in 	Natural) is
      Sec_Parameter : Natural := Rounds;
      DK_Block_Count : Natural;
      Rest : Natural;
      Result_Bytes : Bytes(0..DK_Len-1) := (others => 0 );
      Temp_Bytes : Bytes(0..19);
   begin

      DK_Block_Count := Integer(Float'Ceiling(Float(DK_Len) / 20.0));
      Rest := DK_Len - (DK_Block_Count-1) * 20;

      --TODO: missing handling for uneven rest
      for I in 0..DK_Block_Count-1 loop

         if(I = DK_Block_Count-1)
         then
            Temp_Bytes := To_Bytes(F_Function(Salt     => Salt,
                                           	 Password => Password,
                                           	 Count    => Sec_Parameter,
                                                 Round    => I+1));
            Result_Bytes(I*20..I*20+Rest-1) := Temp_Bytes(0..Rest-1);
         else
            Result_Bytes(I*20..I*20+19) := To_Bytes(F_Function(Salt     => Salt,
                                           	 Password => Password,
                                           	 Count    => Sec_Parameter,
                                                 Round    => I+1));
         end if;





      end loop;

      Key := Result_Bytes;
   end Derive;


   function F_Function(Salt	: in 	Bytes;
                       Password	: in	Bytes;
                       Count	: in 	Natural;
                       Round	: in 	Natural) return W_Block160 is
      Result_Block : W_Block160 := (others =>0);
      Temp_Block : W_Block160 := (others =>0);
      Temp_Bytes : Bytes(0..63) := (others =>0);
      Temp_Bytes_Old : Bytes(0..63) := (others =>0);
   begin

      Temp_Bytes(0..Password'Length-1):=Password;
      Hmac_Package.Init(Key => To_W_Block512(Temp_Bytes));

      --------

      Temp_Bytes := (others => 0);
      Temp_Bytes(0..Salt'Length-1) := Salt;
      Temp_Bytes(Salt'Length..Salt'Length+3):= To_Bytes(Word(Round))(0..3);




--        Ada.Text_IO.Put_Line("Initial Bytes:");
--           for I in Temp_Bytes'Range loop
--              Ada.Text_IO.Put(To_Hex(B => Temp_Bytes(I)));
--           end loop;
--           Ada.Text_IO.New_Line;

      Hmac_Package.Final_Sign(Final_Message_Block        => To_W_Block512(Temp_Bytes),
                              Final_Message_Block_Length => Salt'Length+4,
                              Tag                        => Temp_Block);

      Result_Block:= Temp_Block;

--        Ada.Text_IO.Put_Line("HMAC used 1");
      for I in 2..Count loop
         Temp_Bytes := (others => 0);
         Temp_Bytes(0..19) := To_Bytes(W => Temp_Block);
         Hmac_Package.Final_Sign(Final_Message_Block        => To_W_Block512(Temp_Bytes),
                                 Final_Message_Block_Length => 20,
                                 Tag                        => Temp_Block);

         Result_Block := Result_Block xor Temp_Block;

      end loop;

      return Result_Block;
   end;

   procedure Derive(Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural) is
   begin
      Derive(Salt     => To_Bytes(Salt),
             Password => To_Bytes(Password),
             Key      => Key,
             DK_Len   => DK_Len);

   end Derive;




end Crypto.Symmetric.KDF_PBKDF2;
