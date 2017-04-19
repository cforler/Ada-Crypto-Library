with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package Crypto.Types.Random_Source.Provider is
   package Rnd renames Crypto.Types.Random_Source;

   type Random_Source_Provider is new Rnd.Random_Source with private;
   type Random_Source_Provider_Access is access Random_Source_Provider;

   -----------------
   -- Subprograms --
   -----------------

   overriding
   procedure Initialize (This : in out Random_Source_Provider);

   procedure Initialize (This : in out Random_Source_Provider; File_Path : in String);

   overriding
   procedure Read (This : in out Random_Source_Provider; B : out Byte);

   overriding
   procedure Read (This : in out Random_Source_Provider; Byte_Array : out Bytes);

   overriding
   procedure Read (This : in out Random_Source_Provider; B : out B_Block128);

   overriding
   procedure Read (This : in out Random_Source_Provider; W : out Word);

   overriding
   procedure Read (This : in out Random_Source_Provider; Word_Array : out Words);

   overriding
   procedure Read (This : in out Random_Source_Provider; D : out DWord);

   overriding
   procedure Read (This : in out Random_Source_Provider; DWord_Array : out DWords);

   overriding
   procedure Finalize (This : in out  Random_Source_Provider);

-------
private
-------

   type File_Access is access Ada.Streams.Stream_IO.File_Type;

   type Random_Source_Provider is new Rnd.Random_Source with
      record
	      Source_Path : Ada.Strings.Unbounded.Unbounded_String;
         Source_File : File_Access;
      end record;

   function Path_Starts_With(This : Random_Source_Provider; S : String)
      return Boolean;

end Crypto.Types.Random_Source.Provider;
