package body Tagatha is

   ------------------
   -- Bits_To_Size --
   ------------------

   function Bits_To_Size (Bits : Natural) return Tagatha_Size is
   begin
      if Bits <= 8 then
         return Size_8;
      elsif Bits <= 16 then
         return Size_16;
      elsif Bits <= 32 then
         return Size_32;
      elsif Bits <= 64 then
         return Size_64;
      else
         raise Constraint_Error with
           "Size of operand (" & Natural'Image (Bits) & " too large " &
         "(maximum is 64)";
      end if;

   end Bits_To_Size;

   ------------
   -- Negate --
   ------------

   function Negate (Cond : Tagatha_Condition) return Tagatha_Condition is
   begin
      case Cond is
         when C_Always =>
            raise Constraint_Error with
              "Can't negate always condition";
         when C_Equal =>
            return C_Not_Equal;
         when C_Not_Equal =>
            return C_Equal;
         when C_Greater =>
            return C_At_Most;
         when C_Less =>
            return C_At_Least;
         when C_At_Most =>
            return C_Less;
         when C_At_Least =>
            return C_Greater;
      end case;
   end Negate;

   ---------------
   -- Size_Bits --
   ---------------

   function Size_Bits (Size : Tagatha_Size) return Natural is
   begin
      return 2**(Tagatha_Size'Pos (Size) + 3);
   end Size_Bits;

end Tagatha;
