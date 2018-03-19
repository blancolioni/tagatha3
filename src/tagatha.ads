package Tagatha is

   pragma Pure (Tagatha);

   type Tagatha_Size is (Default_Size,
                         Default_Integer_Size, Default_Address_Size,
                         Size_8, Size_16, Size_32, Size_64);
   pragma Ordered (Tagatha_Size);

   function Size_Bits (Size : Tagatha_Size) return Natural;
   function Bits_To_Size (Bits : Natural) return Tagatha_Size;

   type Local_Offset is new Positive;
   type Argument_Offset is new Positive;

   type Tagatha_Segment is (Executable, Read_Only, Read_Write);

   type Tagatha_Operator is
     (Op_Nop,
      Op_Add, Op_Sub, Op_Mul, Op_Div, Op_Mod,
      Op_And, Op_Or, Op_Xor,
      Op_Equal, Op_Not_Equal,
      Op_Greater, Op_Less,
      Op_Greater_Equal, Op_Less_Equal,
      Op_Bit_Test, Op_Bit_Clear, Op_Bit_Set, Op_Bit_Slice,
      Op_Compare, Op_Change_Size,
      Op_Negate, Op_Not, Op_Complement, Op_Test,
      Op_Dereference);

   subtype Multiplication_Operator is
     Tagatha_Operator range Op_Mul .. Op_Mod;

   subtype Zero_Argument_Operator is
     Tagatha_Operator range Op_Nop .. Op_Nop;

   subtype One_Argument_Operator is
     Tagatha_Operator range Op_Negate .. Op_Dereference;

   subtype Two_Argument_Operator is
     Tagatha_Operator range Op_Add .. Op_Change_Size;

   type Tagatha_Condition is
     (C_Always,
      C_Equal, C_Not_Equal,
      C_Greater, C_Less,
      C_At_Least, C_At_Most);

   function Negate (Cond : Tagatha_Condition) return Tagatha_Condition;

   type Tagatha_Integer is mod 2 ** 64;
   type Tagatha_Integer_Array is array (Positive range <>) of Tagatha_Integer;

   type Tagatha_Floating_Point is new Float;
   type Tagatha_Floating_Point_Array is
     array (Positive range <>) of Tagatha_Floating_Point;

   type Floating_Point_Integer is mod 2 ** 32;

end Tagatha;
