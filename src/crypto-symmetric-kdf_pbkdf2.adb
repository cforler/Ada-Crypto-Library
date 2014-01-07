with Ada.Text_IO;




package body Crypto.Symmetric.KDF_PBKDF2 is

   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	W_Block512) is
      B : Bytes(0..63);
   begin
      Derive(This     => This,
             Salt     => Salt,
             Password => Password,
             Key      => B,
             DK_Len   => 64);
      Key := To_W_Block512(B);
   end Derive;

   function Initialize(This	: out PBKDF2_KDF;
                       Parameter: in Natural) return Boolean is
   begin
      This.Security_Parameter := Parameter;
      return true;
   end Initialize;


   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural) is

      hlen : Integer := Hmac_Package.H.Hash_Type'Size/8;

      Sec_Parameter : Natural := This.Security_Parameter;
      DK_Block_Count : Natural;
      Rest : Natural;
      Result_Bytes : Bytes(0..DK_Len-1) := (others => 0 );
      Temp_Bytes : Bytes(0..hlen-1);

   begin

      Ada.Text_IO.Put_Line("HLEN: " & Integer'Image(hlen));


      DK_Block_Count := Integer(Float'Ceiling(Float(DK_Len) / Float(hlen)));
      Rest := DK_Len - (DK_Block_Count-1) * hlen;

      for I in 0..DK_Block_Count-1 loop

         if(I = DK_Block_Count-1)
         then
            Temp_Bytes := To_Bytes(F_Function(Salt     => Salt,
                                           	 Password => Password,
                                           	 Count    => Sec_Parameter,
                                                 Round    => I+1));
            Result_Bytes(I*hlen..I*hlen+Rest-1) := Temp_Bytes(0..Rest-1);
         else
            Result_Bytes(I*hlen..I*hlen+hlen-1) := To_Bytes(F_Function(Salt     => Salt,
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
                       Round	: in 	Natural) return Hmac_Package.H.Hash_Type is
      Result_Block : Hmac_Package.H.Hash_Type ;
      Temp_Block : Hmac_Package.H.Hash_Type;
      mlen : Integer := Hmac_Package.H.Message_Type'Size/8;
      Temp_Bytes : Bytes(0..mlen-1) := (others =>0);
      Temp_Bytes_Old : Bytes(0..mlen-1) := (others =>0);
      hlen : Integer := Hmac_Package.H.Hash_Type'Size/8;
   begin

      Temp_Bytes(0..Password'Length-1):=Password;
      Hmac_Package.Init(Key => To_Message_Type(Temp_Bytes));

      --------

      Temp_Bytes := (others => 0);
      Temp_Bytes(0..Salt'Length-1) := Salt;
      Temp_Bytes(Salt'Length..Salt'Length+3):= To_Bytes(Word(Round))(0..3);

      Hmac_Package.Final_Sign(Final_Message_Block        => To_Message_Type(Temp_Bytes),
                              Final_Message_Block_Length => Hmac_Package.H.Message_Block_Length_Type(Salt'Length+4),
                              Tag                        => Temp_Block);

      Result_Block:= Temp_Block;

      for I in 2..Count loop
         Temp_Bytes := (others => 0);
         Temp_Bytes(0..hlen-1) := Crypto.Symmetric.KDF_PBKDF2.To_Bytes(Temp_Block);
         Hmac_Package.Final_Sign(Final_Message_Block        => To_Message_Type(Temp_Bytes),
                                 Final_Message_Block_Length => Hmac_Package.H.Message_Block_Length_Type(hlen),
                                 Tag                        => Temp_Block);

         Result_Block := Result_Block xor Temp_Block;

      end loop;

      return Result_Block;
   end;

   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural) is
   begin
      Derive(This     => This,
             Salt     => To_Bytes(Salt),
             Password => To_Bytes(Password),
             Key      => Key,
             DK_Len   => DK_Len);

   end Derive;




end Crypto.Symmetric.KDF_PBKDF2;
