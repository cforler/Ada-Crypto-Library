------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A U N I T . M E M O R Y                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
--                     Copyright (C) 2008-2010, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

--  Memory allocation implementation using the gnat runtime methods.

package body AUnit.Memory is

   -----------
   -- Alloc --
   -----------

   function AUnit_Alloc (Size : size_t) return System.Address is
      function RT_Malloc (Size : size_t) return System.Address;
      pragma Import (C, RT_Malloc, "__gnat_malloc");
   begin
      return RT_Malloc (Size);
   end AUnit_Alloc;

   ----------
   -- Free --
   ----------

   procedure AUnit_Free (Obj : System.Address) is
      procedure RT_Free (Obj : System.Address);
      pragma Import (C, RT_Free, "__gnat_free");
   begin
      RT_Free (Obj);
   end AUnit_Free;

end AUnit.Memory;
