package body Crypto.Types.Random_Source.Provider is
   
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize(This : in out Random_Source_Provider; Name : in Wide_String) is 
   begin
      if Assert (CryptAcquireContext (This.Handle'Address, Name'Address, 0, PROV_RSA_FULL, 0)) then return; end if;
      if GetLastError /= NTE_BAD_KEYSET then raise Windows_Crypto_Failure; end if;
      Ignore( CryptAcquireContext (This.Handle'Address, Name'Address, 0, PROV_RSA_FULL, CRYPT_NEWKEYSET));
    end Initialize;
    
    procedure Initialize(This : in out Random_Source_Provider) is
    begin
      Initialize (This, "AdaCryptoLibrary");
    end Initialize;

   ---------------
   -- Read Byte --
   ---------------

   procedure Read(This : in out Random_Source_Provider; B : out Byte) is begin
      Ignore (CryptGenRandom (This.Handle, 1, B'Address));
   end Read;

   procedure Read(This : in out Random_Source_Provider; Byte_Array : out Bytes) is
   begin
      Ignore (CryptGenRandom (This.Handle, Byte_Array'Length, Byte_Array'Address));
   end Read;

   --------------------
   -- Read Block 128 --
   --------------------

   procedure Read(This : in out Random_Source_Provider; B : out B_Block128) is
   begin
      Ignore (CryptGenRandom (This.Handle, B'Length, B'Address));
   end Read;

   ---------------
   -- Read Word --
   ---------------

   procedure Read(This : in out Random_Source_Provider; W : out Word) is
   begin
      Ignore (CryptGenRandom (This.Handle, 4, W'Address));
   end Read;

   procedure Read(This : in out Random_Source_Provider; Word_Array : out Words) is
   begin
      Ignore (CryptGenRandom (This.Handle, 4 * Word_Array'Length, Word_Array'Address));
   end Read;

   ----------------
   -- Read DWord --
   ----------------

   procedure Read(This : in out Random_Source_Provider; D : out DWord) is
   begin
      Ignore (CryptGenRandom (This.Handle, 8, D'Address));
   end Read;

   procedure Read(This : in out Random_Source_Provider; DWord_Array : out DWords) is
   begin
      Ignore (CryptGenRandom (This.Handle, 8 * DWord_Array'Length, DWord_Array'Address));
   end Read;

   --------------
   -- Finalize --
   --------------

   procedure Finalize(This : in out  Random_Source_Provider) is
   begin
      Ignore (CryptReleaseContext(This.Handle, 0));
   end Finalize;

   ------------
   -- Ignore --
   ------------

   procedure Ignore(C_Succeeded: Int) is begin null; end Ignore;

   ------------
   -- Assert --
   ------------

   function Assert(C_Succeeded: Int) return Boolean is begin return C_Succeeded /= 0; end Assert;

end Crypto.Types.Random_Source.Provider;
