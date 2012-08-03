package body Crypto.Symmetric.MAC.CMAC is

   L    : C.Block;
   U,U2 : C.Block;
   T    : C.Block;

   function Generate_Constants(Value : Bytes) return Block is
      Bytes_Per_Block: constant Positive := Block'Size/8;
   begin
      if (Value(Value'First) and 16#80#) = 0 then -- is MSB of L / L.u zero?
         return Shift_Left(To_Block_Type(Value), 1); -- change Shift_Left in- and output to Bytes?
      else
         declare
            Const: Bytes(0..Bytes_Per_Block-1) := (others => 0);
         begin
            if Bytes_Per_Block = 8 then -- message blocksize n = 64 bit
               Const(Const'Last) := 16#1b#;
               return Shift_Left(To_Block_Type(Value), 1) xor To_Block_Type(Const);
            elsif Bytes_Per_Block = 16 then -- n = 128 bit
               Const(Const'Last) := 16#87#;
               return Shift_Left(To_Block_Type(Value), 1) xor To_Block_Type(Const);
            else
               raise Blocklength_Not_Supported;
            end if;
         end;
      end if;

   end Generate_Constants;

   ---------------------------------------------------------------------------

   procedure Init(Key : in Key_Type) is
      Bytes_Per_Block: constant Positive := Block'Size/8;
      IV: constant Bytes(0..Bytes_Per_Block-1) := (others => 0);
   begin
      C.Prepare_Key(Key);
      C.Encrypt(To_Block_Type(IV), L);
      U := Generate_Constants(To_Bytes(L));
      U2 := Generate_Constants(To_Bytes(U));

      T := T xor T; -- Reset CMAC
   end Init;

   ---------------------------------------------------------------------------

   procedure Sign(Message_Block : in Block) is
   begin
      C.Encrypt(T xor Message_Block, T);
   end Sign;

   ---------------------------------------------------------------------------

   procedure Final_Sign(Final_Message_Block : in  Block;
                        Bytes_Read          : in  Natural;
                        Tag                 : out Block) is
      X: Block;
   begin
      if Bytes_Read = Block'Size / 8 then
         X := Final_Message_Block xor T xor U;
      else -- padding
         declare
            M: Bytes := To_Bytes(Final_Message_Block);
         begin
            M(Bytes_Read) := 16#80#;
            X := To_Block_Type(M) xor T xor U2;
         end;
      end if;

      C.Encrypt(X, Tag);
      T := T xor T; -- Reset CMAC
   end Final_Sign;

   ---------------------------------------------------------------------------

   procedure Verify(Message_Block : in Block) is
   begin
      C.Encrypt(T xor Message_Block, T);
   end Verify;

   ---------------------------------------------------------------------------

   function Final_Verify(Final_Message_Block : in Block;
                         Bytes_Read          : in Natural;
                         Tag                 : in Block)
                         return Boolean is
      X: Block;
      Result : Boolean;
   begin
      if Bytes_Read = Block'Size / 8 then
         X := Final_Message_Block xor T xor U;
      else -- padding
         declare
            M: Bytes := To_Bytes(Final_Message_Block);
         begin
            M(Bytes_Read) := 16#80#;
            X := To_Block_Type(M) xor T xor U2;
         end;
      end if;

      C.Encrypt(X, T);
      Result := (Tag = T);
      T := T xor T; -- Reset CMAC
      return Result;
   end  Final_Verify;

   ---------------------------------------------------------------------------

   procedure Sign(Message : in  Blocks;
                  Key     : in  Key_Type;
                  Tag     : out Block) is
      Final_Message_Block: constant Bytes := To_Bytes(Message(Message'Last));
      Bytes_Read: Natural := Final_Message_Block'Length;
   begin
      Init(Key);

      for I in Message'First..Message'Last-1 loop
         Sign(Message(I));
      end loop;

      for I in reverse Final_Message_Block'Range loop
         if Final_Message_Block(I) = 0 then
            Bytes_Read := I;
         end if;
      end loop;

      Final_Sign(Message(Message'Last), Bytes_Read, Tag);
      T := T xor T; -- Reset CMAC
   end Sign;

   ---------------------------------------------------------------------------

   function Verify(Message : in Blocks;
                   Key     : in Key_Type;
                   Tag     : in Block) return Boolean is
      Result : Boolean;
      Final_Message_Block: constant Bytes := To_Bytes(Message(Message'Last));
      Bytes_Read: Natural := Final_Message_Block'Length;
   begin
      Init(Key);

      for I in Message'First..Message'Last-1 loop
         Verify(Message(I));
      end loop;

      for I in reverse Final_Message_Block'Range loop
         if Final_Message_Block(I) = 0 then
            Bytes_Read := I;
         end if;
      end loop;

      Result := Final_Verify(Message(Message'Last), Bytes_Read, Tag);
      T := T xor T; -- Reset CMAC
      return Result;
   end Verify;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.MAC.CMAC;
