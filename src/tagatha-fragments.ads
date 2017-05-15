private with Ada.Containers.Vectors;

with Tagatha.Commands;

private with Tagatha.Operands;

with Tagatha.Units;

package Tagatha.Fragments is

   --  note: all stack offsets are in _words_

   type Tagatha_Fragment is private;

   function Integer_Constant
     (Value    : in Tagatha_Integer;
      Size  : in Tagatha_Size := Default_Integer_Size)
     return Tagatha_Fragment;

   function Reference_Local
     (Offset   : in Local_Offset;
      Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment;

   function Reference_Argument
     (Offset   : in Argument_Offset;
      Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment;

   function Reference_Result
     (Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment;

   function Reference_External
     (Name      : String;
      Immediate : Boolean;
      Size      : Tagatha_Size  := Default_Integer_Size)
      return Tagatha_Fragment;

   function Push return Tagatha_Fragment;

   function Pop return Tagatha_Fragment;

   function Operator (Op    : Tagatha_Operator;
                      Negate : Boolean         := False;
                      Size  : Tagatha_Size     := Default_Integer_Size)
                     return Tagatha_Fragment;

   function Condition (Cond  : Tagatha_Condition) return Tagatha_Fragment;

   function Compare (Cond   : Tagatha_Condition;
                     Size   : Tagatha_Size := Default_Integer_Size)
                    return Tagatha_Fragment;

   function Branch (Target           : Positive;
                    Branch_Condition : Boolean  := True)
                   return Tagatha_Fragment;

   function Command (Cmd : Tagatha.Commands.Tagatha_Command)
                    return Tagatha_Fragment;

   function Label
     (Index : Positive)
      return Tagatha_Fragment;

   procedure Append (To_Fragment : in out Tagatha_Fragment;
                     Fragment    : in     Tagatha_Fragment);

   procedure Check_Source_Location
     (Fragment     : in out Tagatha_Fragment;
      Line, Column : Positive);

   procedure Append_To_Unit
     (Unit     : in out Tagatha.Units.Tagatha_Unit'Class;
      Fragment : in     Tagatha_Fragment);

   Empty_Fragment : constant Tagatha_Fragment;

   function Show (Fragment : Tagatha_Fragment) return String;

private

   type Fragment_Record_Type is
     (Command_Fragment, Operand_Fragment,
      Condition_Fragment, Branch_Fragment,
      Pop_Fragment, Push_Fragment, Label_Fragment);

   type Fragment_Record
     (Fragment_Type : Fragment_Record_Type := Command_Fragment) is
      record
         Line, Column : Natural := 0;
         case Fragment_Type is
            when Command_Fragment =>
               Command       : Tagatha.Commands.Tagatha_Command;
            when Operand_Fragment =>
               Reference        : Tagatha.Operands.Tagatha_Operand;
               Size             : Tagatha_Size;
            when Condition_Fragment =>
               Condition        : Tagatha_Condition;
            when Branch_Fragment =>
               Branch_Target    : Positive;
               Branch_Condition : Boolean;
            when Pop_Fragment =>
               null;
            when Push_Fragment =>
               null;
            when Label_Fragment =>
               Label_Index      : Positive;
         end case;
      end record;

   package Fragment_Vector is
      new Ada.Containers.Vectors (Positive,
                                  Fragment_Record,
                                  "=");

   type Tagatha_Fragment is
      record
         Records : Fragment_Vector.Vector;
      end record;

   Empty_Fragment : constant Tagatha_Fragment :=
     (Records => Fragment_Vector.Empty_Vector);

end Tagatha.Fragments;
