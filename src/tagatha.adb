package body Tagatha is

   ------------------
   -- Bits_To_Size --
   ------------------

   function Bits_To_Size (Bits : Natural) return Tagatha_Size is
   begin
      return (Tagatha_Custom_Size, (Bits + 7) / 8);
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
      return Size_Octets (Size) * 8;
   end Size_Bits;

   -----------------
   -- Size_Octets --
   -----------------

   function Size_Octets (Size : Tagatha_Size) return Natural is
   begin
      case Size.Category is
         when Tagatha_Default_Size =>
            return 4;
         when Tagatha_Integer_Size =>
            return 4;
         when Tagatha_Address_Size =>
            return 4;
         when Tagatha_Custom_Size =>
            return Size.Octets;
      end case;
   end Size_Octets;

end Tagatha;
