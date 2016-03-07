with Ada.Streams.Stream_IO;

package body Crypto.Types.Random_Source.File is
   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   ------------------------ Initialization -----------------------------------
   ---------------------------------------------------------------------------
   
   
   procedure Initialize(This : in out Random_Source_File) is
   begin
      This.Source_Path := To_Unbounded_String("/dev/random");
   end Initialize;
   
   ---------------------------------------------------------------------------
   
   procedure Initialize(This : in out Random_Source_File;
			File_Path : in String) is
   begin
      This.Source_Path := To_Unbounded_String(File_Path);
   end Initialize;
   
   ---------------------------------------------------------------------------
   ------------------------------- Read Byte ---------------------------------
   ---------------------------------------------------------------------------
   
   
   procedure Read(This : in out Random_Source_File; B : out Byte) is 
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      Byte'Read( Ada.Streams.Stream_IO.Stream(Source_File), B );
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;
   
   ---------------------------------------------------------------------------
      
   procedure Read(This : in out Random_Source_File; Byte_Array : out Bytes) is
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      for I in Byte_Array'Range
      loop
         Byte'Read( Ada.Streams.Stream_IO.Stream(Source_File), Byte_Array(I) );
      end loop;
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;
   
   ---------------------------------------------------------------------------

   procedure Read(This : in out Random_Source_File; B : out B_Block128) is
   begin   
      Crypto.Types.Random_Source.File.Read(This,Bytes(B));
   end Read;  
   
   ---------------------------------------------------------------------------
   ------------------------------- Read Word ---------------------------------
   ---------------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source_File; W : out Word) is 
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      Word'Read( Ada.Streams.Stream_IO.Stream(Source_File), W );
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;
   
   ---------------------------------------------------------------------------
   
   procedure Read(This : in out Random_Source_File; Word_Array : out Words) is 
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      for I in Word_Array'Range
      loop
         Word'Read( Ada.Streams.Stream_IO.Stream(Source_File), Word_Array(I) );
      end loop;
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;
   
   ---------------------------------------------------------------------------
   ------------------------------- Read DWord --------------------------------
   ---------------------------------------------------------------------------
   

   procedure Read(This : in out Random_Source_File; D : out DWord) is 
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      DWord'Read( Ada.Streams.Stream_IO.Stream(Source_File), D );
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;
   
   procedure Read(This : in out Random_Source_File; DWord_Array : out DWords) is 
      Source_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open(
         Source_File, 
         Ada.Streams.Stream_IO.In_File, 
         To_String(This.Source_Path)
      );
      for I in DWord_Array'Range
      loop
         DWord'Read( Ada.Streams.Stream_IO.Stream(Source_File), DWord_Array(I) );
      end loop;
      Ada.Streams.Stream_IO.Close(Source_File);
   end Read;

end Crypto.Types.Random_Source.File;
