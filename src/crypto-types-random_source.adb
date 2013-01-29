package body Crypto.Types.Random_Source is
   
   procedure Read(This : in out Random_Source; Byte_Array : out Bytes) is
   begin
      for I in Byte_Array'Range loop
	 Read(Random_Source'class(This),Byte_Array(I));
      end loop;
   end Read;
   
   ----------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source; B : out B_Block128) is 
   begin
      for I in B'Range loop
	 Read(Random_Source'class(This),B(I));
      end loop;
   end Read;
   
   ----------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source; W : out Word) is
      B : Byte_Word;
   begin
      This.Read(Bytes(B));
      W := To_Word(B);
   end Read;	 
   
   ----------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source; Word_Array : out Words) is
   begin
      for I in Word_Array'Range loop
	 This.Read(Word_Array(I));
      end loop;
   end Read;
   
   ----------------------------------------------------------------------
   
  procedure Read(This : in out Random_Source; D : out DWord) is
      B : Byte_DWord;
   begin
      This.Read(Bytes(B));
      D := To_DWord(B);
   end Read;	 
   
   ----------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source; DWord_Array : out DWords) is
   begin
      for I in DWord_Array'Range loop
	 This.Read(DWord_Array(I));
      end loop;
   end Read;
   
end Crypto.Types.Random_Source;
