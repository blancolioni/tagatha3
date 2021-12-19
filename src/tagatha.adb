package body Tagatha is

   ------------------
   -- Bits_To_Size --
   ------------------

   function Bits_To_Size (Bits : Natural) return Tagatha_Size is
   begin
      return (Tagatha_Custom_Size, (Bits + 7) / 8);
   end Bits_To_Size;

   ----------------
   -- Data_Image --
   ----------------

   function Data_Image (Data : Tagatha_Data_Type) return String is
   begin
      return (case Data is
                 when Untyped_Data => "",
                 when Address_Data => "a",
                 when Floating_Point_Data => "f");
   end Data_Image;

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

   ----------------
   -- Size_Image --
   ----------------

   function Size_Image (Size : Tagatha_Size) return String is

      function Custom_Size_Image return String;

      -----------------------
      -- Custom_Size_Image --
      -----------------------

      function Custom_Size_Image return String is
         Cust : constant String := Size.Octets'Image;
      begin
         return Cust (Cust'First + 1 .. Cust'Last);
      end Custom_Size_Image;

      Img : constant String :=
              (case Size.Category is
                  when Tagatha_Default_Size        => "",
                  when Tagatha_Integer_Size        => "i",
                  when Tagatha_Address_Size        => "a",
                  when Tagatha_Floating_Point_Size => "f",
                  when Tagatha_Custom_Size         =>
                    Custom_Size_Image);
   begin
      return Img;
   end Size_Image;

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
         when Tagatha_Floating_Point_Size =>
            return 4;
         when Tagatha_Custom_Size =>
            return Size.Octets;
      end case;
   end Size_Octets;

end Tagatha;
