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


package body  Crypto.Symmetric.Compression is

  function Compress( Number : in Integer;
                        i1    : in Digest_Type;
                       i2    : in Digest_Type) return Digest_Type is
  output : Digest_Type;
  begin
  Generic_Compress(Number,i1, i2, output);
  return Output;
  end Compress;

   ---------------------------------------------------------------------------

   function To_Bytes(input : Digest_Type) return Bytes is
   begin
      return Generic_To_Bytes(input);
   end To_Bytes;

    ---------------------------------------------------------------------------

   function To_Digest(input : Bytes) return Digest_Type is
   begin
      return Generic_To_Digest(input);
   end To_Digest;

  -----------------------------------------------------------------------------
  
  procedure resetState is
  begin
    Generic_resetState;
  end resetState;

   end Crypto.Symmetric.Compression;
