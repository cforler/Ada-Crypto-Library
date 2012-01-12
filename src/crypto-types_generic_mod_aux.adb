package body Crypto.Types_Generic_Mod_Aux is
   function "xor"(Left, Right : T_A) return T_A is
      Result : T_A(0..Left'Length-1);
   begin
      if Left'Length /= Right'Length then
	 raise  Constraint_Error;
      end if;
      for I in 0..Left'Length-1 loop
         Result(I) := Left(Left'First+I) xor Right(Right'First+I);
      end loop;
      return Result;
   end "xor";
   
   
   ------------------------------------------------------------------------
   
   function "xor"(Left : T_A; Right : T) return T_A is
      Result : T_A := Left;
   begin
      Result(Result'Last) := Left(Result'Last) xor Right;
      return Result;
   end "xor";
   
   ------------------------------------------------------------------------
   
   
   function "xor"(Left : T; Right : T_A) return T_A is
   begin
      return Right xor Left;
   end "xor";
   
   ------------------------------------------------------------------------
   function "+"(Left : T_A; Right : T) return T_A is
      Result: T_A(Left'Range) := Left;
   begin
      Result(Left'Last) := Left(Left'Last) + Right;

      -- overflow?
      if Result(Left'Last) < Left(Left'Last) then
         for I in reverse Left'First..Left'Last-1 loop
            Result(I):=Result(I)+1;
            if Result(I) /= 0 then
               return  Result;
            end if;
         end loop;
      end if;
      return Result;
   end "+";

   
   ------------------------------------------------------------------------
   function "+"(Left : T; Right : T_A) return T_A is
   begin
      return Right + Left;
   end "+";
   ------------------------------------------------------------------------
   
   function Is_Zero(Item : T_A) return Boolean is
   begin
      for I in  Item'Range loop
         if Item(I) /= 0 then return False;
         end if;
      end loop;
      return True;
   end Is_Zero;
   
   ------------------------------------------------------------------------
   
   function Left_Part(Block : in T_A) return T_A is
      Len  : constant Natural := ((Block'Length+1)/2)-1;
      Left : constant T_A(0..Len) := Block(Block'First..(Block'First+Len));
   begin
      return Left;
   end Left_Part;
   
   ------------------------------------------------------------------------
   
   function Right_Part(Block : in T_A) return T_A is
      Len : constant Natural := Block'Length/2;	
      Right : constant T_A(0..Len-1) := Block(Block'Last-Len+1..Block'Last);
   begin
      return Right;
   end Right_Part;
   
   ------------------------------------------------------------------------
   
end Crypto.Types_Generic_Mod_Aux;
