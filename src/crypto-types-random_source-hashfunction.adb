with Crypto.Types.Random_Source.Provider;

package body Crypto.Types.Random_Source.Hashfunction is
   
   procedure Initialize(This : in out Random_Source_Hashfunction) is
      Rand : Crypto.Types.Random_Source.Provider.Random_Source_Provider;
      Seed : B_Hash;
   begin
      Rand.Read(Seed);
      This.Reset(Seed);
   end;
   
   procedure Initialize(This : in out Random_Source_Hashfunction;
			Seed : in B_Hash) is
   begin
      This.Reset(Seed);
   end Initialize; 
   
   
   procedure Read(This : in out Random_Source_Hashfunction; B : out Byte) is
   begin
      if(This.Index = 0) then 
	 This.Update;
      end if;
      B := This.Buffer(This.Index);
      This.Index:= This.Index-1;
   end Read; 
   
   
   procedure Read(This : in out Random_Source_Hashfunction;
		  Byte_Array : out Bytes) is
      Len : Natural := Byte_Array'Length; 
      Out_Index : Natural := Byte_Array'First;
   begin
      while Len > L loop
	 Byte_Array(Out_Index..(Out_Index+L-1)) := H.To_Bytes(H.Hash(This.Input));
	 This.Input :=  This.Input+1;
	 Len := Len - L;
      end loop;
      
      if  Len > 0 then
	 if Len <= This.Index then
	    Byte_Array(Out_Index..Byte_Array'Last) := This.Buffer(This.Index-Len+1..This.Index);
	    This.Index:= This.Index-Len;
	 else
	    Byte_Array(Out_Index..Out_Index+This.Index-1) := This.Buffer(1..This.Index);
	    This.Update;	 
	    Out_Index := Out_Index+This.Index;
	    Len := Len - This.Index;
	    Byte_Array(Out_Index..Byte_Array'Last) := This.Buffer(1..Len);
	 end if;
      end if;
   end Read;
   
   
   procedure Read(This : in out Random_Source_Hashfunction; B : out B_Block128) is
   begin
      This.Read(Bytes(B));
   end Read;
      
     
   
   
   procedure Reset(This : in out Random_Source_Hashfunction;
		   Seed : in B_Hash) is
   begin
      This.Input(1..L) := Seed;
      This.Update;
   end Reset; 
   
   
   procedure Update(This : in out Random_Source_Hashfunction) is
   begin
      This.Buffer := H.To_Bytes(H.Hash(This.Input));
      This.Index := Index_Type'Last;
      This.Input := This.Input+1;
   end Update;	    
		    
   
end Crypto.Types.Random_Source.Hashfunction;
