package body Crypto.Random_Source.File is
   use Interfaces.C_Streams;
   use Ada.Strings.Unbounded;
   use type Interfaces.C_Streams.Size_T;

   ---------------------------------------------------------------------------
   ------------------------ Initialization -----------------------------------
   ---------------------------------------------------------------------------
   
   
   procedure Initialize(This : in out Random_Source_File) is
      Cpath : constant String := "/dev/random" & ASCII.NUL;
      Mode  : constant String := "r";   
   begin
      This.Source_File := Fopen(Cpath'address, Mode'address);
      This.Source_Path := To_Unbounded_String("/dev/random");
   end Initialize;
   
   ---------------------------------------------------------------------------
   
   procedure Initialize(This : in out Random_Source_File;
			File_Path : in String) is
      Cpath : constant String := File_Path & ASCII.NUL;
      Mode  : constant String := "r";   
   begin
      This.Source_File := Fopen(Cpath'address, Mode'address);
      This.Source_Path := To_Unbounded_String(File_Path);
   end Initialize;
   
   ---------------------------------------------------------------------------
   ------------------------------- Read Byte ---------------------------------
   ---------------------------------------------------------------------------
   
   
   procedure Read(This : in Random_Source_File; B : out Byte) is 
      I : Int;
   begin
      I := fgetc(This.Source_File);
      if  I < 0 then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      end if;
      B := Byte(I);
   end Read;
   
   ---------------------------------------------------------------------------
      
--   procedure Read(This : in Random_Source'Class; Byte_Array : out Bytes) is
   procedure Read(This : in Random_Source_File; Byte_Array : out Bytes) is
   begin  
      if  Fread(Buffer => Byte_Array'address, Size => 1, Count => Byte_Array'Length, Stream => This.Source_File) /= Byte_Array'Length then
         raise  RANDOM_SOURCE_READ_ERROR with  To_String(This.Source_Path);
      end if;
   end Read;
   
   ---------------------------------------------------------------------------


   procedure Read(This : in Random_Source_File; B : out B_Block128) is
   begin   
      This.Read(Bytes(B));
   end Read;  
   
   ---------------------------------------------------------------------------
   ------------------------------- Read Word ---------------------------------
   ---------------------------------------------------------------------------
   
   procedure Read(This : in Random_Source_File; W : out Word) is 
   begin  
      if Fread( Buffer => W'Address, Size => W'Size/8, Count => 1, 
		Stream => This.Source_File ) /= 1 then
	 raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      end if;
   end Read;
   
   ---------------------------------------------------------------------------
   
   procedure Read(This : in Random_Source_File; Word_Array : out Words) is 
   begin  
      if Fread(Buffer => Word_Array'Address, Size => Word'Size/8, Count => Word_Array'Length, Stream => This.Source_File) /=  Word_Array'Length  then
	 raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      end if;
   end Read;
   
   ---------------------------------------------------------------------------
   ------------------------------- Read DWord --------------------------------
   ---------------------------------------------------------------------------
   

   procedure Read(This : in Random_Source_File; D : out DWord) is 
   begin  
      if  Fread( Buffer => D'Address, Size => D'Size/8, Count => 1, Stream => This.Source_File) /= 1 then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      end if;
   end Read;
   
   procedure Read(This : in Random_Source_File; DWord_Array : out DWords) is
   begin  
      if  Fread(Buffer => DWord_Array'Address, Size => DWord'Size/8,
		Count => DWord_Array'Length, Stream => This.Source_File) /=  DWord_Array'Length then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      end if;      
   end Read;
   
   
   ---------------------------------------------------------------------------
   ------------------------------- Finalize ----------------------------------
   ---------------------------------------------------------------------------
   
   procedure Finalize(This : in out  Random_Source_File) is
   begin
      if Fileno(This.Source_File) >= 0 then      	
	 if  Fclose(This.Source_File) /= 0 then
	    raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path)
	      &": can not close file.";
	 end if;
      end if;
     end Finalize;
end Crypto.Random_Source.File;
