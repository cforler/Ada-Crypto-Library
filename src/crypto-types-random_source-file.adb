package body Crypto.Types.Random_Source.File is
   use Ada.Strings.Unbounded;
   use Ada.Streams.Stream_IO;

   ---------------------------------------------------------------------------
   ------------------------ Initialization -----------------------------------
   ---------------------------------------------------------------------------


   procedure Initialize(This : in out Random_Source_File) is
      Path : constant String := "/dev/random";
      Mode  : constant File_Mode := In_File;
   begin
      if This.Source_File = null then
         This.Source_File := new Ada.Streams.Stream_IO.File_Type;
      end if;
      if not Is_Open(This.Source_File.all) then
         Open(This.Source_File.all, Mode, Path, "shared=yes");
         This.Source_Path := To_Unbounded_String(Path);
      end if;
   end Initialize;

   ---------------------------------------------------------------------------

   procedure Initialize(This : in out Random_Source_File;
		File_Path : in String) is
      Mode  : constant File_Mode := In_File;
   begin
      if Is_Open(This.Source_File.all) then
         Close(This.Source_File.all);
      end if;
      if not Is_Open(This.Source_File.all) then
         Open(This.Source_File.all, Mode, File_Path, "shared=yes");
         This.Source_Path := To_Unbounded_String(File_Path);
      end if;
    end Initialize;

   ---------------------------------------------------------------------------
   ------------------------------- Read Byte ---------------------------------
   ---------------------------------------------------------------------------


   procedure Read(This : in out Random_Source_File; B : out Byte) is

   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         Byte'Read(Stream(This.Source_File.all), B);
      end if;
   end Read;

   ---------------------------------------------------------------------------

   procedure Read(This : in out Random_Source_File; Byte_Array : out Bytes) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         Bytes'Read(Stream(This.Source_File.all), Byte_Array);
      end if;
   end Read;

   ---------------------------------------------------------------------------

   procedure Read(This : in out Random_Source_File; B : out B_Block128) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         B_Block128'Read(Stream(This.Source_File.all), B);
      end if;
   end Read;

   ---------------------------------------------------------------------------
   ------------------------------- Read Word ---------------------------------
   ---------------------------------------------------------------------------

   procedure Read(This : in out Random_Source_File; W : out Word) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         Word'Read(Stream(This.Source_File.all), W);
      end if;
   end Read;

   ---------------------------------------------------------------------------

   procedure Read(This : in out Random_Source_File; Word_Array : out Words) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         Words'Read(Stream(This.Source_File.all), Word_Array);
      end if;
   end Read;

   ---------------------------------------------------------------------------
   ------------------------------- Read DWord --------------------------------
   ---------------------------------------------------------------------------


   procedure Read(This : in out Random_Source_File; D : out DWord) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         DWord'Read(Stream(This.Source_File.all), D);
      end if;
   end Read;

   procedure Read(This : in out Random_Source_File; DWord_Array : out DWords) is
   begin
      if not Path_Starts_With(This, "/dev/") and then End_Of_File(This.Source_File.all) then
         raise  RANDOM_SOURCE_READ_ERROR with To_String(This.Source_Path);
      else
         DWords'Read(Stream(This.Source_File.all), DWord_Array);
      end if;
   end Read;


   ---------------------------------------------------------------------------
   ------------------------------- Finalize ----------------------------------
   ---------------------------------------------------------------------------

   procedure Finalize(This : in out  Random_Source_File) is
   begin
      if Is_Open(This.Source_File.all) then
         Close(This.Source_File.all);
      end if;
   end Finalize;

   ---------------------------------------------------------------------------
   --------------------------- Path_Starts_With ------------------------------
   ---------------------------------------------------------------------------

   function Path_Starts_With(This : Random_Source_File; S : String) return Boolean is
      Path : constant String := To_String(This.Source_Path);
   begin
      return Path(Path'First..S'Last) = S;
   end;

end Crypto.Types.Random_Source.File;
