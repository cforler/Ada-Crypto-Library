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

with Ada.Unchecked_Conversion;
with Ada.Containers.Doubly_Linked_Lists;
with Crypto.Types;                       use Crypto.Types;

package Crypto.Types.Skein is

   function "+" (Left : DWord; Right : Integer) return DWord;

   function Natural_To_Bytes (N : Natural; number : Natural) return Bytes;

   function Bytes_To_Dword (b : in Bytes) return DWord;

   function Bytes_To_Dwords (b : in Bytes) return DWords;

   function Dword_To_Bytes (s : in DWord) return Bytes;

   function Dwords_To_Bytes (s : in DWords) return Bytes;

   function "+" (Left : Bytes; Right : Natural) return Bytes;



end Crypto.Types.Skein;
