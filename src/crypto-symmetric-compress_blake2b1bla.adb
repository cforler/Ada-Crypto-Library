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

with Crypto.Symmetric.Algorithm.Blake2b1Bla; use Crypto.Symmetric.Algorithm.Blake2b1Bla;

  package body Crypto.Symmetric.Compress_Blake2b1Bla is
  
    function Process(This     : in out Blake2b1Bla_Scheme;
                     Input_1   : in Bytes;
                     Input_2   : in Bytes;
                     vindex    : in Natural) return Bytes is
    Tmp : DW_Block512;
    begin
      Crypto.Symmetric.Algorithm.Blake2b1Bla.Compress(This.Ctx, vindex, to_DW_Block512(Input_1), to_DW_Block512(Input_2), tmp);
      return To_Bytes(Tmp);
    end Process;
    
    function Get_Length(This     : in out Blake2b1Bla_Scheme) return Integer is
    begin
      return 64;
    end Get_Length;

    procedure Reset(This     : in out Blake2b1Bla_Scheme) is
    begin
     ResetState(This.ctx);
    end Reset;

   end Crypto.Symmetric.Compress_Blake2b1Bla;