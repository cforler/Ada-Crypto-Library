with System; use System;
with Interfaces.C; use Interfaces.C;

package Crypto.Types.Random_Source.Provider is
   package Rnd renames Crypto.Types.Random_Source;

   type Random_Source_Provider is new Rnd.Random_Source with private;

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize (This : in out Random_Source_Provider; Name : in Wide_String);

   overriding
   procedure Initialize (This : in out Random_Source_Provider);

   overriding 
   procedure Read(This : in out Random_Source_Provider; B : out Byte);

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

   ----------------
   -- Exceptions --
   ----------------

   Windows_Crypto_Failure : exception;

-------
private
-------

   type Random_Source_Provider is new Rnd.Random_Source with
      record Handle : Address; end record;

   PROV_RSA_FULL   : constant Unsigned_Long := 1;
   CRYPT_NEWKEYSET : constant Unsigned_Long := 8;
   NTE_BAD_KEYSET  : constant Unsigned_Long := 16#80090016#;

   -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa379886(v=vs.85).aspx
   function CryptAcquireContext (
      phProv       : in Address;       -- *HCRYPTPROV
      pszContainer : in Address;       -- LPCTSTR
      pszProvider  : in Unsigned_Long; -- LPCTSTR
      dwProvType   : in Unsigned_Long; -- DWORD
      dwFlags      : in Unsigned_Long) -- DWORD
   return Int
      with Import => True, Convention => StdCall, External_Name => "CryptAcquireContextW";

   -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa379942(v=vs.85).aspx
   function CryptGenRandom (
      hProv    : in Address;       -- HCRYPTPROV
      wLen     : in Unsigned_Long; -- DWORD
      pbBuffer : in Address)       -- BYTE
   return Int
      with Import => True, Convention => StdCall, External_Name => "CryptGenRandom";

   -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa380268(v=vs.85).aspx
   function CryptReleaseContext (
      hProv   : in Address;       -- HCRYPTPROV
      dwFlags : in Unsigned_Long) -- DWORD
   return Int
      with Import => True, Convention => StdCall, External_Name => "CryptReleaseContext";

   -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx
   function GetLastError return Unsigned_Long
      with Import => True, Convention => StdCall, External_Name => "GetLastError";

   procedure Ignore (C_Succeeded: Int);
   function  Assert (C_Succeeded: Int) return Boolean;

   pragma Linker_Options("-ladvapi32");
   pragma Linker_Options("-lkernel32");

end Crypto.Types.Random_Source.Provider;
