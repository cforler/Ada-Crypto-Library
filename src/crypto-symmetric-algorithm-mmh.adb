-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.


package body Crypto.Symmetric.Algorithm.MMH is

   procedure Hash(Key, Message : in Word; Hash : out Word) is
      Sum : constant DWord  := DWord(Key) * DWord(Message);
      Stmp : constant DWord := (Sum  and 16#Ff_Ff_Ff_Ff#) - (Shift_Right(Sum,32)*15);
      Utmp : constant DWord := (Stmp and 16#Ff_Ff_Ff_Ff#) - (Shift_Right(Stmp,32)*15);
   begin
      if Utmp > 16#1_00_00_00_0f# then Hash := Word(Utmp) - 15;
      else Hash := Word(Utmp);
      end if;
   end Hash;
   
   ----------------------------------------------------------------------

   procedure Hash(Key, Message : in Words; Hash : out Word) is
      Sum  : DWord:=0;
      Stmp : DWord;
      Utmp : DWord;
   begin
      if Key'Length /= Message'Length then
         raise  Constraint_Error;
      else
         for I in 0..Key'Length-1 loop
            Sum := Sum +
              (DWord(Key(Key'First+I)) * DWord(Message(Message'First+I)));
         end loop;
      end if;
      
      Stmp := (Sum  and 16#Ff_Ff_Ff_Ff#) - (Shift_Right(Sum,32)*15);
      Utmp := (Stmp and 16#Ff_Ff_Ff_Ff#) - (Shift_Right(Stmp,32)*15);
      if Utmp > 16#1_00_00_00_0f# then Hash := Word(Utmp) - 15;
      else Hash := Word(Utmp);
      end if;
   end Hash;

end Crypto.Symmetric.Algorithm.MMH;

