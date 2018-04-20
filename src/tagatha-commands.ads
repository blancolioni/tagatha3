private with Ada.Strings.Unbounded;

with Tagatha.Labels;
with Tagatha.Operands;

package Tagatha.Commands is

   type Tagatha_Command is private;

   function Push (Operand    : Tagatha.Operands.Tagatha_Operand;
                  Size       : Tagatha_Size := Default_Integer_Size)
                  return Tagatha_Command;

   function Pop  (Operand    : Tagatha.Operands.Tagatha_Operand;
                  Size       : Tagatha_Size     := Default_Integer_Size)
                  return Tagatha_Command;

   function Drop  (Size       : Tagatha_Size     := Default_Integer_Size)
                   return Tagatha_Command;

   function Duplicate return Tagatha_Command;
   function Swap return Tagatha_Command;
   function Store
     (Size : Tagatha_Size := Default_Size)
      return Tagatha_Command;
   function Save return Tagatha_Command;
   function Restore return Tagatha_Command;

   type Copy_Direction is (From, To);

   function Start_Copy (Direction : Copy_Direction) return Tagatha_Command;

   function Copy_Item
     (Size       : Tagatha_Size := Default_Size)
      return Tagatha_Command;

   function End_Copy return Tagatha_Command;

   function Operate (Op   : Tagatha_Operator;
                     Neg  : Boolean           := False;
                     Size : Tagatha_Size      := Default_Integer_Size)
                     return Tagatha_Command;

   function Call (Target : Tagatha.Labels.Tagatha_Label)
                  return Tagatha_Command
     with Pre => Tagatha.Labels.Has_Label (Target);

   function Indirect_Call return Tagatha_Command;

   function Loop_Around (Label        : Tagatha.Labels.Tagatha_Label;
                         Loop_Count   : Local_Offset;
                         Loop_Index   : Local_Offset;
                         Size         : Tagatha_Size := Default_Integer_Size)
                        return Tagatha_Command;

   function Jump (Target : Tagatha.Labels.Tagatha_Label;
                  Cond   : Tagatha_Condition := C_Always;
                  Size   : Tagatha_Size      := Default_Address_Size)
                  return Tagatha_Command
     with Pre => Tagatha.Labels.Has_Label (Target);

   function Native_Command
     (Name              : String;
      Input_Stack_Words  : Natural;
      Output_Stack_Words : Natural;
      Changed_Registers  : String)
      return Tagatha_Command;

   function Show (Command : Tagatha_Command) return String;

   procedure Set_Label (Command : in Tagatha_Command;
                        Label   : in Tagatha.Labels.Tagatha_Label);

   function Get_Label (Command : in Tagatha_Command)
                      return Tagatha.Labels.Tagatha_Label;

   procedure Set_Source_Reference
     (Command : in Tagatha_Command;
      Line, Column : Positive);

private

   type Stack_Operation is (S_Push, S_Pop, S_Drop,
                            S_Duplicate, S_Store, S_Swap);

   type Tagatha_Instruction is
     (T_Stack,     --  push or pop

      T_Operate,   --  operate on stack elements (e.g. add/subtract)

      T_Call,      --  call another unit

      T_Loop,      --  bounded loop

      T_Jump,      --  local jump within this unit

      T_Native     --  a command that should be output literally
     );

   type Tagatha_Command_Record
     (Instruction : Tagatha_Instruction := T_Stack) is
      record
         Size   : Tagatha_Size;
         Label  : Tagatha.Labels.Tagatha_Label;
         Line   : Natural := 0;
         Column : Natural := 0;
         Negate : Boolean;
         case Instruction is
            when T_Stack =>
               Stack_Op          : Stack_Operation;
               Operand           : Tagatha.Operands.Tagatha_Operand;
            when T_Operate =>
               Operator          : Tagatha_Operator;
            when T_Call =>
               Subroutine        : Tagatha.Labels.Tagatha_Label;
               Indirect          : Boolean;
            when T_Loop =>
               Limit             : Local_Offset;
               Counter           : Local_Offset;
               End_Label         : Tagatha.Labels.Tagatha_Label;
            when T_Jump =>
               Condition         : Tagatha_Condition;
               Destination       : Tagatha.Labels.Tagatha_Label;
            when T_Native =>
               Native_Name       : Ada.Strings.Unbounded.Unbounded_String;
               Changed_Registers : Ada.Strings.Unbounded.Unbounded_String;
               Input_Words       : Natural;
               Output_Words      : Natural;
         end case;
      end record;

   type Tagatha_Command is access Tagatha_Command_Record;

   function Stack_Command
     (Op   : Stack_Operation;
      Size : Tagatha_Size := Default_Size)
      return Tagatha_Command;

   function Get_Command_Operator (Command : Tagatha_Command)
                                  return Tagatha_Operator;

   function Get_Jump_Condition (Command : Tagatha_Command)
                                return Tagatha_Condition;

   function Get_Stack_Operand (Command : Tagatha_Command)
                               return Tagatha.Operands.Tagatha_Operand;

   function Get_Stack_Operation (Command : Tagatha_Command)
                                 return Stack_Operation;

   function Duplicate return Tagatha_Command
   is (Stack_Command (S_Duplicate));

   function Swap return Tagatha_Command
   is (Stack_Command (S_Swap));

   function Store
     (Size : Tagatha_Size := Default_Size)
      return Tagatha_Command
   is (Stack_Command (S_Store, Size));

end Tagatha.Commands;
