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
--with Ada.Numerics.Discrete_Random;

with Crypto;
with Crypto.Random_Source.File;

package body Crypto.Random is
   Dev_Random :  Crypto.Random_Source.File.Random_Source_File; 
   
    Rnd_Src :  Crypto.Random_Source.Random_Source'Class
      :=  Crypto.Random_Source.Random_Source'Class(Dev_Random);   
    
    procedure Set(Source : in Crypto.Random_Source.Random_Source'Class) is
    begin
       Rnd_Src := Source;
    end Set;

    procedure Read(B : out Byte) is
    begin
       Rnd_Src.Read(B);
    end Read;

    procedure Read(Byte_Array : out Bytes) is
    begin
       Rnd_Src.Read(Byte_Array);
    end Read;

    procedure Read(B : out B_Block128) is
    begin
       Rnd_Src.Read(B);
    end Read;

    procedure Read(W : out Word) is
    begin
       Rnd_Src.Read(W);
    end Read;

    procedure Read(Word_Array : out Words) is
    begin
       Rnd_Src.Read(Word_Array);
    end Read;

    procedure Read(D : out DWord) is
    begin
       Rnd_Src.Read(D);
    end Read;

    procedure Read(DWord_Array : out DWords) is
    begin
       Rnd_Src.Read(DWord_Array);
    end Read;  
begin
   Dev_Random.Initialize("/dev/random");
   Set( Dev_Random);
end Crypto.Random;
