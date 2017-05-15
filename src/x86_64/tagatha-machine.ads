package Tagatha.Machine is

   Tagatha_Word_Size    : constant Tagatha_Size := Size_32;
   Tagatha_Address_Size : constant Tagatha_Size := Tagatha_Word_Size;
   Tagatha_Address_Bits : constant := 32;

   Tagatha_Floating_Point_Size : constant Tagatha_Size := Size_64;

   Tagatha_Integer_Size : constant := 32;
   type Tagatha_Integer is mod 2**(Tagatha_Integer_Size);

   type Tagatha_Floating_Point is new Long_Float;

   type Floating_Point_Integer is mod 2**64;

   type Tagatha_Integer_Array is array (Positive range <>) of Tagatha_Integer;
   type Tagatha_Floating_Point_Array is
     array (Positive range <>) of Tagatha_Floating_Point;

   type Tagatha_Address is mod 2**Tagatha_Address_Bits;

end Tagatha.Machine;

