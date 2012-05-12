with Interfaces.C_Streams;
with Ada.Strings.Unbounded;

package Crypto.Types.Random_Source.File is
   package Rnd renames Crypto.Types.Random_Source;

   type Random_Source_File is new Rnd.Random_Source with private;
   type Random_Source_File_Access is access  Random_Source_File;
   
   Overriding
   procedure Finalize(This : in out  Random_Source_File);
   
   Overriding
   procedure Initialize(This : in out Random_Source_File);
   
   procedure Initialize(This : in out Random_Source_File;
			File_Path : in String);
   Overriding
   procedure Read(This : in Random_Source_File; B : out Byte);
   
   Overriding 
   procedure Read(This : in Random_Source_File; Byte_Array : out Bytes);
   
   Overriding
   procedure Read(This : in Random_Source_File; B : out B_Block128);
   
   Overriding
   procedure Read(This : in Random_Source_File; W : out Word);
   
   Overriding
   Procedure Read(This : in Random_Source_File; Word_Array : out Words);
   
   Overriding
   procedure Read(This : in Random_Source_File; D : out DWord);
   
   Overriding
   procedure Read(This : in Random_Source_File; DWord_Array : out DWords);
private
   type Random_Source_File is new Rnd.Random_Source with
      record
	 Source_Path : Ada.Strings.Unbounded.Unbounded_String;
	 Source_File : Interfaces.C_Streams.Files;
      end record;
end Crypto.Types.Random_Source.File;
