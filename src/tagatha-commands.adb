with Ada.Strings.Fixed;
--  with Ada.Strings.Fixed;

with Tagatha.Constants;

package body Tagatha.Commands is

   ----------
   -- Call --
   ----------

   function Call (Target         : Tagatha.Labels.Tagatha_Label;
                  Argument_Count : Natural)
                  return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Call,
                                         Tagatha.Labels.No_Label, 0, 0, False,
                                         Target, False, Argument_Count);
   end Call;

   ---------------
   -- Copy_Item --
   ---------------

   function Copy_Item
     (Direction  : Copy_Direction;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size := Default_Size)
      return Tagatha_Command
   is
   begin
      case Direction is
         when From =>
            return Push (Tagatha.Transfers.Iterator_Copy_Operand (Data, Size));
         when To =>
            return Pop (Tagatha.Transfers.Iterator_Copy_Operand (Data, Size));
      end case;
   end Copy_Item;

   -----------------
   -- Dereference --
   -----------------

   function Dereference
     (Data : Tagatha_Data_Type := Untyped_Data;
      Size : Tagatha_Size      := Default_Size)
      return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (Instruction       => T_Dereference,
         Data              => Data,
         Size              => Size,
         others            => <>);
   end Dereference;

   ----------
   -- Drop --
   ----------

   function Drop  (Size       : Tagatha_Size     := Default_Integer_Size)
                   return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (T_Stack,
         Tagatha.Labels.No_Label,
         0, 0, False,
         S_Drop,
         Transfers.Constant_Operand
           (Tagatha.Constants.Integer_Constant (0), Size));
   end Drop;

   --------------------------
   -- Get_Command_Operator --
   --------------------------

   function Get_Command_Operator (Command : Tagatha_Command)
                                  return Tagatha_Operator
   is
   begin
      return Command.Operator;
   end Get_Command_Operator;

   ------------------------
   -- Get_Jump_Condition --
   ------------------------

   function Get_Jump_Condition (Command : Tagatha_Command)
                                return Tagatha_Condition
   is
   begin
      return Command.Condition;
   end Get_Jump_Condition;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Command : in Tagatha_Command)
                      return Tagatha.Labels.Tagatha_Label
   is
   begin
      return Command.Label;
   end Get_Label;

   -----------------------
   -- Get_Stack_Operand --
   -----------------------

   function Get_Stack_Operand (Command : Tagatha_Command)
                               return Tagatha.Transfers.Transfer_Operand
   is
   begin
      return Command.Operand;
   end Get_Stack_Operand;

   -------------------------
   -- Get_Stack_Operation --
   -------------------------

   function Get_Stack_Operation (Command : Tagatha_Command)
                                 return Stack_Operation
   is
   begin
      return Command.Stack_Op;
   end Get_Stack_Operation;

   -------------------
   -- Indirect_Call --
   -------------------

   function Indirect_Call
     (Argument_Count : Natural)
      return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (Instruction       => T_Call,
         Label             => Tagatha.Labels.No_Label,
         Line              => 0,
         Column            => 0,
         Negate            => False,
         Subroutine        => Tagatha.Labels.No_Label,
         Indirect          => True,
         Arguments         => Argument_Count);
   end Indirect_Call;

   ----------
   -- Jump --
   ----------

   function Jump (Target : Tagatha.Labels.Tagatha_Label;
                  Cond   : Tagatha_Condition := C_Always)
                  return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Jump,
                                         Tagatha.Labels.No_Label,
                                         0, 0, False, Cond, Target);
   end Jump;

   -----------------
   -- Loop_Around --
   -----------------

   function Loop_Around (Label        : Tagatha.Labels.Tagatha_Label;
                         Loop_Count   : Local_Offset;
                         Loop_Index   : Local_Offset)
                         return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Loop,
                                         Tagatha.Labels.No_Label,
                                         0, 0, False,
                                         Loop_Count, Loop_Index, Label);
   end Loop_Around;

   --------------------
   -- Native_Command --
   --------------------

   function Native_Command
     (Name              : String;
      Input_Stack_Words  : Natural;
      Output_Stack_Words : Natural;
      Changed_Registers  : String)
      return Tagatha_Command
   is
--        Rs : String_Vectors.Vector;
--        Start : Positive := Changed_Registers'First;
--        Index : Natural := Ada.Strings.Fixed.Index (Changed_Registers, ",");
   begin
--        while Index > 0 loop
--           Rs.Append (Changed_Registers (Start .. Index));
--           Start := Index + 1;
--           Index := Ada.Strings.Fixed.Index (Changed_Registers, ",", Start);
--        end loop;
--        if Start <= Changed_Registers'Last then
--           Rs.Append (Changed_Registers (Start .. Changed_Registers'Last));
--        end if;

      return new Tagatha_Command_Record'
        (T_Native,
         Tagatha.Labels.No_Label,
         1, 1, False,
         Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Ada.Strings.Unbounded.To_Unbounded_String (Changed_Registers),
         Input_Stack_Words, Output_Stack_Words);
   end Native_Command;

   -------------
   -- Operate --
   -------------

   function Operate (Op   : Tagatha_Operator;
                     Neg  : Boolean           := False)
                     return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (Instruction       => T_Operate,
         Label             => Tagatha.Labels.No_Label,
         Line              => 0,
         Column            => 0,
         Negate            => Neg,
         Operator          => Op);
   end Operate;

   ---------
   -- Pop --
   ---------

   function Pop
     (Operand    : Tagatha.Transfers.Transfer_Operand)
      return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (T_Stack,
         Tagatha.Labels.No_Label,
         0, 0, False,
         S_Pop, Operand);
   end Pop;

   ----------
   -- Push --
   ----------

   function Push
     (Operand    : Tagatha.Transfers.Transfer_Operand)
      return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'
        (T_Stack,
         Tagatha.Labels.No_Label,
         0, 0, False,
         S_Push, Operand);
   end Push;

   -------------
   -- Restore --
   -------------

   function Restore return Tagatha_Command is
   begin
      return Push (Tagatha.Transfers.Shelf_Operand ("_"));
   end Restore;

   ----------
   -- Save --
   ----------

   function Save return Tagatha_Command is
   begin
      return Pop (Tagatha.Transfers.Shelf_Operand ("_"));
   end Save;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (Command : in Tagatha_Command;
                        Label   : in Tagatha.Labels.Tagatha_Label)
   is
   begin
      Command.Label := Label;
   end Set_Label;

   --------------------------
   -- Set_Source_Reference --
   --------------------------

   procedure Set_Source_Reference
     (Command : in Tagatha_Command;
      Line, Column : Positive)
   is
   begin
      Command.Line := Line;
      Command.Column := Column;
   end Set_Source_Reference;

   ----------
   -- Show --
   ----------

   function Show (Command : Tagatha_Command) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      Label : Labels.Tagatha_Label := Command.Label;
      Label_Text : Unbounded_String;
      Command_Text : constant String :=
                       (case Command.Instruction is
                           when T_Stack   =>
                          (case Command.Stack_Op is
                              when S_Push =>
                                 "push " &
                                 Tagatha.Transfers.Show (Command.Operand),
                              when S_Pop  =>
                                 "pop  " &
                                 Tagatha.Transfers.Show (Command.Operand),
                              when S_Drop =>
                                 "drop",
                              when S_Duplicate =>
                                 "dup",
                              when S_Store     =>
                                 "store",
                              when S_Swap      =>
                                 "swap"),
                           when T_Operate =>
                             Command.Operator'Image,
                           when T_Dereference =>
                             "@"
                        & Data_Image (Command.Data)
                        & "." & Size_Image (Command.Size),
                           when T_Call    =>
                          (if Command.Indirect
                           then "call (indirect)"
                           else "call " &
                             Tagatha.Labels.Show (Command.Subroutine, 'L')),
                           when T_Loop    =>
                              "loop" &
                              Command.Limit'Img & Command.Counter'Img &
                          " " & Tagatha.Labels.Show (Command.End_Label, 'L'),
                           when T_Jump    =>
                              "jump " & Command.Condition'Img &
                              " " &
                          Tagatha.Labels.Show (Command.Destination, 'L'),
                           when T_Native  =>
                              Ada.Strings.Unbounded.To_String
                          (Command.Native_Name));
   begin
      while Tagatha.Labels.Has_Label (Label) loop
         Label_Text := Label_Text & Labels.Show (Label, 'L') & ":";
         Label := Tagatha.Labels.Next_Linked_Label (Label);
      end loop;
      return To_String (Label_Text)
        & "[" & Trim (Command.Line'Img, Ada.Strings.Left)
        & "," & Trim (Command.Column'Img, Ada.Strings.Left) & "]"
        & Command_Text;
   end Show;

   -------------------
   -- Stack_Command --
   -------------------

   function Stack_Command
     (Op      : Stack_Operation;
      Operand : Tagatha.Transfers.Transfer_Operand :=
        Tagatha.Transfers.No_Operand)
      return Tagatha_Command
   is
   begin
      return new Tagatha_Command_Record'(T_Stack,
                                         Tagatha.Labels.No_Label,
                                         0, 0, False,
                                         Op, Operand);
   end Stack_Command;

   ---------------------
   -- Start_Iteration --
   ---------------------

   function Start_Iteration return Tagatha_Command is
   begin
      return Pop (Tagatha.Transfers.Iterator_New_Operand);
   end Start_Iteration;

end Tagatha.Commands;
