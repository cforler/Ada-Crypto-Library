with Crypto.Symmetric.Hashfunction;

generic
   with package H is new Crypto.Symmetric.Hashfunction(<>);
   
package Crypto.Types.Random_Source.Hashfunction is
   package Rnd renames Crypto.Types.Random_Source;

   type Random_Source_Hashfunction is new Rnd.Random_Source with private;
   type Random_Source_Hashfunction_Access is access  Random_Source_Hashfunction;
   
   L : constant Natural := H.Hash_Type'Size/8;
   
   subtype B_Hash is Bytes(0.. L-1);
   
   Overriding
   procedure Initialize(This : in out Random_Source_Hashfunction);
   
   procedure Initialize(This : in out Random_Source_Hashfunction;
			Seed : in B_Hash);
     
   Overriding
   procedure Read(This : in out Random_Source_Hashfunction; B : out Byte);
   
   Overriding 
   procedure Read(This : in out Random_Source_Hashfunction;
		  Byte_Array : out Bytes);
   
   Overriding
   procedure Read(This : in out Random_Source_Hashfunction; B : out B_Block128);
     
   procedure Reset(This : in out Random_Source_Hashfunction;
		   Seed : in B_Hash);
   
       
private
   subtype Index_Type is Natural range 0..L;
     
   type Random_Source_Hashfunction is new Rnd.Random_Source with
      record 
	 -- Seed + 64 Bit Counter
	 Input :  Bytes(1..L+8) := (others => 0);
	 
	 Buffer :  B_Hash := (others => 0);
	 Index : Index_Type := 0;
      end record;
   
   procedure Update(This : in out Random_Source_Hashfunction);
   
end Crypto.Types.Random_Source.Hashfunction;
