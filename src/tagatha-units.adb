with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Tagatha.Code;
with Tagatha.Commands.Registry;
with Tagatha.Constants;
with Tagatha.File_Assembly;
with Tagatha.Transfers.Optimiser;

with Tagatha.Registry;
with Ada.Strings.Fixed;

package body Tagatha.Units is

   Trace_Commands : constant Boolean := False;

   procedure Allocate_Registers
     (Unit : Tagatha_Unit;
      Target : Tagatha.Code.Translator'Class);

   procedure Write
     (Unit   : in     Tagatha_Unit;
      Target : in out Tagatha.Code.Translator'Class;
      Directory_Path : String);

   procedure Append (To_Unit : in out Tagatha_Unit;
                     Command : in     Tagatha.Commands.Tagatha_Command);

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                By          : in     Positive         := 1);

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                For_Segment : in     Tagatha_Segment;
                                By          : in     Positive         := 1);

   --------------------
   -- Allocate_Local --
   --------------------

   procedure Allocate_Local
     (Unit : in out Tagatha_Unit'Class)
   is
      Offset : constant Local_Offset := Unit.Allocate_Local;
   begin
      pragma Unreferenced (Offset);
   end Allocate_Local;

   --------------------
   -- Allocate_Local --
   --------------------

   function Allocate_Local
     (Unit : in out Tagatha_Unit)
      return Local_Offset
   is
      Sub : constant Tagatha_Subprogram := Unit.Current_Sub;
   begin
      Sub.Temporary_Words := Sub.Temporary_Words + 1;
      return Local_Offset
        (Sub.Frame_Words + Sub.Temporary_Words);
   end Allocate_Local;

   ------------------------
   -- Allocate_Registers --
   ------------------------

   procedure Allocate_Registers
     (Unit   : Tagatha_Unit;
      Target : Tagatha.Code.Translator'Class)
   is
      Base_Address : Natural := 0;
      First        : constant Positive := 1;
      Last         : Natural := 0;
   begin
      for Data in Tagatha_Data_Type loop
         declare
            Regs : constant Tagatha.Code.Register_Range_Record :=
                     Target.Get_Register_Range (Data);
         begin
            Last := Natural'Max (Last, Regs.Last);
         end;
      end loop;

      declare
         Allocation : Transfers.Register_Allocation_Array (First .. Last);
      begin
         for Data in Tagatha_Data_Type loop
            declare
               Regs : constant Tagatha.Code.Register_Range_Record :=
                        Target.Get_Register_Range (Data);
            begin
               for I in Regs.First .. Regs.Last loop
                  Allocation (I).Allowed_Data_Type (Data) := True;
               end loop;
            end;
         end loop;

         for Sub of Unit.Subprograms loop
            for Offset in 1 .. Sub.Transfers.Last_Index loop
               declare
                  Address  : constant Positive := Base_Address + Offset;
               begin
                  Tagatha.Transfers.Reference_Temporaries
                    (Sub.Transfers (Offset), Address);
               end;
            end loop;
            Base_Address := Base_Address + Sub.Executable_Segment.Last_Index;
         end loop;

         for Sub of Unit.Subprograms loop
            Sub.Temporary_Words := 0;
            for Offset in 1 .. Sub.Transfers.Last_Index loop
               Tagatha.Transfers.Assign_Registers
                 (Sub.Transfers (Offset), Allocation, Sub.Temporary_Words);
            end loop;
         end loop;
      end;
   end Allocate_Registers;

   ------------
   -- Append --
   ------------

   procedure Append (To_Unit : in out Tagatha_Unit;
                     Command : in     Tagatha.Commands.Tagatha_Command)
   is
   begin
      if Tagatha.Labels.Has_Label (To_Unit.Last_Label (Executable)) then
         declare
            Label : constant Labels.Tagatha_Label :=
                      To_Unit.Last_Label (Executable);
         begin
            Tagatha.Commands.Set_Label (Command, Label);
            To_Unit.Last_Label (Executable) := Tagatha.Labels.No_Label;
         end;
      end if;

      Commands.Set_Source_Reference
        (Command, To_Unit.Current_Line, To_Unit.Current_Column);
      To_Unit.Current_Sub.Executable_Segment.Append (Command);

      if Trace_Commands then
         Ada.Text_IO.Put_Line
           (Tagatha.Commands.Show (Command));
      end if;

      Increment_Address (To_Unit, Executable);
   end Append;

   ------------------
   -- Ascii_String --
   ------------------

   procedure Ascii_String (Unit  : in out Tagatha_Unit;
                           Value : in     String)
   is
   begin
      Unit.Current_Sub.Read_Only_Segment.Append
        (Data_Directive'
           (String_Data, Unit.Last_Label (Unit.Current_Segment), Size_8,
            Ada.Strings.Unbounded.To_Unbounded_String (Value)));
      Increment_Address (Unit, Read_Only);
   end Ascii_String;

   ------------------
   -- Asciz_String --
   ------------------

   procedure Asciz_String (Unit  : in out Tagatha_Unit;
                           Value : in     String)
   is
   begin
      Ascii_String (Unit, Value & Character'Val (0));
   end Asciz_String;

   ----------------
   -- Begin_Code --
   ----------------

   procedure Begin_Code
     (Unit           : in out Tagatha_Unit;
      Name           : in     String;
      Global         : in     Boolean)
   is
   begin
      Unit.Begin_Routine (Name, 0, 0, Global);
      Unit.Current_Sub.Has_Frame := False;
   end Begin_Code;

   -----------------
   -- Begin_Frame --
   -----------------

   procedure Begin_Frame
     (Unit        : in out Tagatha_Unit;
      Frame_Words : in     Natural)
   is
      Current_Size : Natural := 0;
   begin
      Unit.Current_Sub.Frames.Append (Frame_Words);
      for Frame of Unit.Current_Sub.Frames loop
         Current_Size := Current_Size + Frame;
      end loop;
      Unit.Current_Sub.Frame_Words :=
        Natural'Max (@, Current_Size);
   end Begin_Frame;

   -------------------
   -- Begin_Routine --
   -------------------

   procedure Begin_Routine
     (Unit           : in out Tagatha_Unit;
      Name           : in     String;
      Argument_Words : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean)
   is
   begin
      Unit.Current_Sub := new Tagatha_Subprogram_Record;
      Unit.Subprograms.Append (Unit.Current_Sub);
      Unit.Current_Sub.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Unit.Current_Sub.Argument_Words := Argument_Words;
      Unit.Current_Sub.Frame_Words := 0;
      Unit.Current_Sub.Result_Words := Result_Words;
      Unit.Current_Sub.Temporary_Words := 0;
      Unit.Current_Sub.Global := Global;
   end Begin_Routine;

   ----------
   -- Call --
   ----------

   procedure Call
     (Unit           : in out Tagatha_Unit;
      Target         : in     String;
      Argument_Count : Natural)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Target, Import => True);
      Append (Unit, Commands.Call (Label, Argument_Count));
   end Call;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property
     (Unit  : in out Tagatha_Unit;
      Name  : String)
   is
   begin
      if Unit.Properties.Contains (Name) then
         Unit.Properties.Delete (Name);
      end if;
   end Clear_Property;

   -------------
   -- Command --
   -------------

   procedure Command (Unit    : in out Tagatha_Unit;
                      Command : in     Tagatha.Commands.Tagatha_Command)
   is
   begin
      Append (Unit, Command);
   end Command;

   ---------------
   -- Copy_Item --
   ---------------

   procedure Copy_From
     (Unit : in out Tagatha_Unit;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size := Default_Size)
   is
   begin
      Append (Unit, Commands.Copy_Item (Tagatha.Commands.From, Data, Size));
   end Copy_From;

   -------------
   -- Copy_To --
   -------------

   procedure Copy_To
     (Unit : in out Tagatha_Unit;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size := Default_Size)
   is
   begin
      Append (Unit, Commands.Copy_Item (Tagatha.Commands.To, Data, Size));
   end Copy_To;

   -----------------
   -- Create_Unit --
   -----------------

   procedure Create_Unit
     (New_Unit       : in out Tagatha_Unit;
      Name           : in     String;
      Source_File    : in     String)
   is
      use Ada.Strings.Unbounded;
      Unit : Tagatha_Unit;
   begin
      Unit.Name := To_Unbounded_String (Name);
      Unit.Source_File := To_Unbounded_String (Source_File);
      New_Unit := Unit;
   end Create_Unit;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
      New_Value : constant Data_Directive :=
                    (Integer_Data, Unit.Last_Label (Unit.Current_Segment),
                     Size, Value);
   begin
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
         when Executable | Read_Only =>
            Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
         when Read_Write =>
            Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size     := Default_Integer_Size)
   is
      Label : Tagatha.Labels.Tagatha_Label;
      New_Value : Data_Directive;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => True);
      New_Value :=
        (Label_Data, Unit.Last_Label (Unit.Current_Segment),
         Size, Label);
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
         when Executable | Read_Only =>
            Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
         when Read_Write =>
            Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer_Array;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
   begin
      for I in Value'Range loop
         Data (Unit, Value (I), Size);
      end loop;
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point)
   is
      New_Value : constant Data_Directive :=
                    (Floating_Point_Data,
                     Unit.Last_Label (Unit.Current_Segment),
                     Default_Size, Value);
   begin
      Unit.Last_Label (Unit.Current_Segment) := Tagatha.Labels.No_Label;
      case Unit.Current_Segment is
      when Executable | Read_Only =>
         Unit.Current_Sub.Read_Only_Segment.Append (New_Value);
      when Read_Write =>
         Unit.Current_Sub.Read_Write_Segment.Append (New_Value);
      end case;
      Increment_Address (Unit);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point_Array)
   is
   begin
      for I in Value'Range loop
         Data (Unit, Value (I));
      end loop;
   end Data;

   ----------------------
   -- Deallocate_Local --
   ----------------------

   procedure Deallocate_Local
     (Unit : in out Tagatha_Unit)
   is
   begin
      Unit.Current_Sub.Temporary_Words :=
        Unit.Current_Sub.Temporary_Words - 1;
   end Deallocate_Local;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Unit : in out Tagatha_Unit;
      Data : Tagatha_Data_Type := Untyped_Data;
      Size : Tagatha_Size  := Default_Integer_Size) is
   begin
      Append (Unit,
              Tagatha.Commands.Dereference (Data, Size));
   end Dereference;

   ---------------
   -- Directive --
   ---------------

   procedure Directive (Unit : in out Tagatha_Unit;
                        Value : String;
                        Address : Integer := -1)
   is
   begin
      if Unit.Current_Sub /= null then
         Unit.Current_Sub.Directives.Append
           ((Unit.Current_Segment,
            (if Address < 0
             then Unit.Current_Sub.Next_Address (Unit.Current_Segment)
             else Address),
            Ada.Strings.Unbounded.To_Unbounded_String (Value)));
      else
         Unit.Directives.Append
           ((Unit.Current_Segment,
            0,
            Ada.Strings.Unbounded.To_Unbounded_String (Value)));
      end if;
   end Directive;

   ----------
   -- Drop --
   ----------

   procedure Drop (Unit      : in out Tagatha_Unit;
                   Size      : in     Tagatha_Size := Default_Size)
   is
   begin
      Append (Unit, Commands.Drop (Size));
   end Drop;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate (Unit : in out Tagatha_Unit) is
   begin
      Append (Unit, Commands.Duplicate);
   end Duplicate;

   -----------------
   -- End_Routine --
   -----------------

   procedure End_Code
     (Unit : in out Tagatha_Unit)
   is
   begin
      Unit.End_Routine;
   end End_Code;

   --------------
   -- End_Copy --
   --------------

   procedure End_Copy
     (Unit : in out Tagatha_Unit)
   is null;

   ---------------
   -- End_Frame --
   ---------------

   procedure End_Frame
     (Unit        : in out Tagatha_Unit)
   is
   begin
      Unit.Current_Sub.Frames.Delete_Last;
   end End_Frame;

   -----------------
   -- End_Routine --
   -----------------

   procedure End_Routine
     (Unit : in out Tagatha_Unit)
   is
   begin
      if Labels.Has_Label (Unit.Last_Label (Executable)) then
         declare
            Nop : constant Tagatha.Commands.Tagatha_Command :=
                    Tagatha.Commands.Operate
                      (Op_Nop, False);
         begin
            Commands.Set_Label (Nop, Unit.Last_Label (Executable));
            Unit.Last_Label (Executable) := Labels.No_Label;
            Unit.Current_Sub.Executable_Segment.Append (Nop);
            Increment_Address (Unit, Executable, 1);
         end;
      end if;

      Unit.Optimise;

   end End_Routine;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Unit : Tagatha_Unit) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Unit.Name;
   begin
      for I in 1 .. Length (Result) loop
         if Element (Result, I) = '.' then
            Replace_Slice (Result, I, I, "__");
         end if;
      end loop;
      return To_Lower (To_String (Result));
   end External_Name;

   ----------------------
   -- File_System_Name --
   ----------------------

   function File_System_Name (Unit : Tagatha_Unit) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded, Ada.Strings.Maps;
   begin
      return To_Lower
        (To_String (Translate (Unit.Name, To_Mapping (".", "-"))));
   end File_System_Name;

   -----------------
   -- Finish_Unit --
   -----------------

   procedure Finish_Unit (Unit : in out Tagatha_Unit) is
   begin
      pragma Unreferenced (Unit);
   end Finish_Unit;

   -----------------------
   -- Increment_Address --
   -----------------------

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                By          : in     Positive         := 1)
   is
   begin
      Increment_Address (For_Unit, For_Unit.Current_Segment, By);
   end Increment_Address;

   -----------------------
   -- Increment_Address --
   -----------------------

   procedure Increment_Address (For_Unit    : in out Tagatha_Unit;
                                For_Segment : in     Tagatha_Segment;
                                By          : in     Positive         := 1)
   is
   begin
      For_Unit.Current_Sub.Next_Address (For_Segment) :=
        For_Unit.Current_Sub.Next_Address (For_Segment) + By;
   end Increment_Address;

   -------------------
   -- Indirect_Call --
   -------------------

   procedure Indirect_Call
     (Unit           : in out Tagatha_Unit;
      Argument_Count : Natural)
   is
   begin
      Append (Unit, Commands.Indirect_Call (Argument_Count));
   end Indirect_Call;

   ----------
   -- Jump --
   ----------

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     Integer;
                   Condition : in     Tagatha_Condition := C_Always)

   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Target);
      Append (Unit, Tagatha.Commands.Jump (Label, Condition));
   end Jump;

   ----------
   -- Jump --
   ----------

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     String)

   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label, Target,
                                      Import => True);
      Append (Unit, Tagatha.Commands.Jump (Label, C_Always));
   end Jump;

   -----------
   -- Label --
   -----------

   procedure Label (Unit   : in out Tagatha_Unit;
                    Name   : in     String;
                    Export : in     Boolean := False)
   is
      use Tagatha.Labels;
      Label : Tagatha_Label;
   begin
      Create_Label
        (In_List    => Unit.Labels,
         Label      => Label,
         Linked_To  => Unit.Last_Label (Unit.Current_Segment),
         Segment    => Unit.Current_Segment,
         Location   => Unit.Current_Sub.Next_Address (Unit.Current_Segment),
         Name       => Name,
         Export     => Export);
      Unit.Last_Label (Unit.Current_Segment) := Label;
   end Label;

   -----------
   -- Label --
   -----------

   procedure Label (Unit   : in out Tagatha_Unit;
                    Index  : in     Positive)
   is
      use Tagatha.Labels;
      Label : Tagatha_Label;
   begin
      Create_Label
        (In_List    => Unit.Labels,
         Label      => Label,
         Linked_To  => Unit.Last_Label (Unit.Current_Segment),
         Location   => Unit.Current_Sub.Next_Address (Executable),
         Index      => Index);
      Unit.Last_Label (Unit.Current_Segment) := Label;
   end Label;

   -----------------
   -- Loop_Around --
   -----------------

   procedure Loop_Around (Unit         : in out Tagatha_Unit;
                          Label_Name   : in     String;
                          Loop_Count   : in     Local_Offset;
                          Loop_Index   : in     Local_Offset)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name);
      Append (Unit,
              Tagatha.Commands.Loop_Around (Label, Loop_Count, Loop_Index));
   end Loop_Around;

   ----------------------------
   -- Native_Stack_Operation --
   ----------------------------

   procedure Native_Operation
     (Unit               : in out Tagatha_Unit;
      Name               : String;
      Input_Stack_Words  : Natural := 0;
      Output_Stack_Words : Natural := 0;
      Changed_Registers  : String  := "")
   is
   begin
      Unit.Command
        (Commands.Native_Command
           (Name, Input_Stack_Words, Output_Stack_Words, Changed_Registers));
   end Native_Operation;

   ----------------
   -- Next_Label --
   ----------------

   function Next_Label (Unit   : in out Tagatha_Unit) return Positive is
      Result : constant Positive := Unit.Next_Label;
   begin
      Unit.Next_Label :=
        Unit.Next_Label + 1;
      return Result;
   end Next_Label;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Unit   : in out Tagatha_Unit;
      Op     : Tagatha_Operator)
   is
   begin
      Append (Unit, Tagatha.Commands.Operate (Op, False));
   end Operate;

   --------------
   -- Optimise --
   --------------

   procedure Optimise (Unit : in out Tagatha_Unit) is
      Registry : Tagatha.Registry.Tagatha_Registry;
      Start_Label : Tagatha.Labels.Tagatha_Label;
   begin

      Tagatha.Labels.Create_Label
        (In_List    => Unit.Labels,
         Label      => Start_Label,
         Linked_To  => Tagatha.Labels.No_Label,
         Segment    => Executable,
         Location   => 1,
         Name       =>
           Ada.Strings.Unbounded.To_String
             (Unit.Name)
           & "__"
           & Ada.Strings.Unbounded.To_String
              (Unit.Current_Sub.Name),
         Export     => True);

      Registry.Start
        (Unit_Label => Start_Label,
         Size       => Unit.Current_Sub.Frame_Words);

      for I in 1 .. Unit.Current_Sub.Next_Address (Executable) - 1 loop
         declare
            Command : constant Tagatha.Commands.Tagatha_Command :=
              Unit.Current_Sub.Executable_Segment.Element (I);
         begin
            Tagatha.Commands.Registry.Register_Command
              (Registry, Command);
         end;
      end loop;

      Tagatha.Registry.Get_Transfers
        (Registry, Unit.Current_Sub.Transfers);

      Tagatha.Transfers.Optimiser.Optimise (Unit.Current_Sub.Transfers);

   end Optimise;

   -----------------
   -- Pop_Address --
   -----------------

   procedure Pop_Address
     (Unit       : in out Tagatha_Unit;
      Address    : String;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size := Default_Integer_Size;
      External   : Boolean      := False)
   is
   begin
      Append (Unit,
              Commands.Pop
                (Transfers.Dereference
                   (Transfers.External_Operand
                        (Address,
                         Immediate => True,
                         Data      => Data,
                         Size      => Size))));
   end Pop_Address;

   ------------------
   -- Pop_Argument --
   ------------------

   procedure Pop_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop
                (Transfers.Argument_Operand (Offset, False, Data, Size)));
   end Pop_Argument;

   ---------------
   -- Pop_Label --
   ---------------

   procedure Pop_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : String;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size := Default_Integer_Size;
      External   : Boolean      := False)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => External);
      Append (Unit,
              Commands.Pop (Transfers.Label_Operand
                (Label, Untyped_Data, Size)));
   end Pop_Label;

   ---------------
   -- Pop_Local --
   ---------------

   procedure Pop_Local
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop
                (Transfers.Local_Operand (Offset, False, Data, Size)));
   end Pop_Local;

   -----------------
   -- Pop_Operand --
   -----------------

   procedure Pop_Operand
     (Unit      : in out Tagatha_Unit;
      Op        : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      Append (Unit, Commands.Pop (Op));
   end Pop_Operand;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Pop_Result
     (Unit       : in out Tagatha_Unit;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Transfers.Result_Operand (Data, Size)));
   end Pop_Result;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Constant_Operand
                   (Tagatha.Constants.Integer_Constant (Value),
                    Size => Size)));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Constant_Operand
                   (Tagatha.Constants.Floating_Point_Constant (Value),
                    Size => Default_Floating_Point_Size)));
   end Push;

   -------------------
   -- Push_Argument --
   -------------------

   procedure Push_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Argument_Operand
                   (Offset, Size => Size)));
   end Push_Argument;

   ---------------------------
   -- Push_Argument_Address --
   ---------------------------

   procedure Push_Argument_Address
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Argument_Operand
                   (Offset, Indirect => True,
                 Data => Address_Data, Size => Default_Address_Size)));
   end Push_Argument_Address;

   ----------------
   -- Push_Label --
   ----------------

   procedure Push_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size;
      External   : in     Boolean       := False)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => External);
      Append (Unit,
              Commands.Push
                (Transfers.Label_Operand
                   (Label, Data => Data, Size => Size)));
   end Push_Label;

   ------------------------
   -- Push_Label_Address --
   ------------------------

--     procedure Push_Label_Address
--       (Unit       : in out Tagatha_Unit;
--        Label_Name : in     String)
--     is
--        Label : Tagatha.Labels.Tagatha_Label;
--     begin
--        Tagatha.Labels.Reference_Label (Unit.Unit.Labels, Label,
--                                        Label_Name);
--        Append (Unit, Make_Stack_Operation (S_Push, Tagatha_Address_Size,
--          (O_Label, True, Label)));
--     end Push_Label_Address;

   ----------------
   -- Push_Local --
   ----------------

   procedure Push_Local
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Local_Operand
                   (Offset, Data => Data, Size => Size)));
   end Push_Local;

   ------------------------
   -- Push_Local_Address --
   ------------------------

   procedure Push_Local_Address
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset)
   is
   begin
      Append (Unit,
              Commands.Push
                (Transfers.Local_Operand
                   (Offset, Indirect => True,
                    Data             => Address_Data,
                    Size             => Default_Address_Size)));
   end Push_Local_Address;

   ------------------
   -- Push_Operand --
   ------------------

   procedure Push_Operand
     (Unit      : in out Tagatha_Unit;
      Op        : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      Append (Unit, Commands.Push (Op));
   end Push_Operand;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Push_Result
     (Unit       : in out Tagatha_Unit;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Transfers.Result_Operand (Data, Size)));
   end Push_Result;

   -----------------
   -- Push_Return --
   -----------------

   procedure Push_Return
     (Unit       : in out Tagatha_Unit;
      Data       : Tagatha_Data_Type := Untyped_Data;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Transfers.Return_Operand (Data, Size)));
   end Push_Return;

   ---------------
   -- Push_Text --
   ---------------

   procedure Push_Text
     (Unit : in out Tagatha_Unit;
      Text : String)
   is
      Index_Image : String := Integer'Image (Unit.Next_String);
      Label       : constant String :=
                      "__tagatha$string_"
                      & Unit.Unit_Label_Name
                      & Ada.Strings.Fixed.Trim (Index_Image, Ada.Strings.Left);
   begin
      Unit.Next_String := Unit.Next_String + 1;
      Index_Image (Index_Image'First) := '_';

      Unit.Segment (Read_Only);
      Unit.Label (Label);
      Unit.Data (Text'Length);

      for Ch of Text loop
         Unit.Data (Character'Pos (Ch));
      end loop;

      Unit.Segment (Tagatha.Executable);
      Unit.Push_Label (Label);
   end Push_Text;

   ------------------------
   -- Push_Local_Address --
   ------------------------

--     procedure Push_Local_Address
--       (Unit       : in out Tagatha_Unit;
--        Offset     : in     Local_Offset)
--     is
--     begin
--        Append (Unit, Make_Stack_Operation (S_Push, Tagatha_Address_Size,
--          (O_Local, True, Offset)));
--     end Push_Local_Address;

   -----------------
   -- Restore_Top --
   -----------------

   procedure Restore_Top (Unit : in out Tagatha_Unit) is
   begin
      Append (Unit, Commands.Restore);
   end Restore_Top;

   --------------
   -- Save_Top --
   --------------

   procedure Save_Top (Unit : in out Tagatha_Unit) is
   begin
      Append (Unit, Commands.Save);
   end Save_Top;

   -------------
   -- Segment --
   -------------

   procedure Segment (Unit  : in out Tagatha_Unit;
                      Seg   : in     Tagatha_Segment)
   is
   begin
      Unit.Current_Segment := Seg;
   end Segment;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Unit  : in out Tagatha_Unit;
      Name  : String;
      Value : String)
   is
   begin
      if Unit.Properties.Contains (Name) then
         Unit.Properties.Replace (Name, Value);
      else
         Unit.Properties.Insert (Name, Value);
      end if;
   end Set_Property;

   -----------------
   -- Source_Line --
   -----------------

   procedure Source_Location
     (Unit   : in out Tagatha_Unit;
      Line   : Positive;
      Column : Positive)
   is
   begin
      Unit.Current_Line := Line;
      Unit.Current_Column := Column;
   end Source_Location;

   ----------------
   -- Start_Copy --
   ----------------

   procedure Start_Copy
     (Unit      : in out Tagatha_Unit)
   is
   begin
      Append (Unit, Commands.Start_Iteration);
   end Start_Copy;

   -----------
   -- Store --
   -----------

   procedure Store
     (Unit : in out Tagatha_Unit;
      Size : Tagatha_Size := Default_Size)
   is
   begin
      Append (Unit, Commands.Store (Size));
   end Store;

   ----------
   -- Swap --
   ----------

   procedure Swap (Unit : in out Tagatha_Unit) is
   begin
      Append (Unit, Commands.Swap);
   end Swap;

   ---------------------
   -- Unit_Label_Name --
   ---------------------

   function Unit_Label_Name (Unit : Tagatha_Unit'Class) return String is
      Result : String := Unit.Unit_Name;
   begin
      for Ch of Result loop
         if Ch = '-' then
            Ch := '_';
         end if;
      end loop;
      return Result;
   end Unit_Label_Name;

   -----------
   -- Write --
   -----------

   procedure Write
     (Unit   : in     Tagatha_Unit;
      Target : in out Tagatha.Code.Translator'Class;
      Directory_Path : String)
   is
      use Tagatha.File_Assembly;
      use type Tagatha.Labels.Tagatha_Label;
      File_Path : constant String :=
                    Directory_Path
                    & "/" & Unit.File_System_Name
                    & Target.Extension;
      File   : File_Assembly_Type;
   begin
      Open (File, File_Path);
      Target.File_Preamble (File_Assembly_Type'Class (File),
                            Ada.Strings.Unbounded.To_String
                              (Unit.Source_File));

      for Directive of Unit.Directives loop
         File.Put_Line (Ada.Strings.Unbounded.To_String (Directive.Value));
      end loop;

      for Sub of Unit.Subprograms loop

         declare
            use List_Of_Directives;
            Directive : Cursor := Sub.Directives.First;
         begin

            while Has_Element (Directive)
              and then Element (Directive).Index < 1
            loop
               File.Put_Line
                 (Ada.Strings.Unbounded.To_String
                    (Element (Directive).Value));
               Next (Directive);
            end loop;

            Target.Start (File_Assembly_Type'Class (File),
                          Subprogram_Name (Sub), True);

            if Sub.Has_Frame then
               Target.Begin_Frame (File_Assembly_Type'Class (File),
                                   Return_Count    => Sub.Result_Words,
                                   Arg_Count       => Sub.Argument_Words,
                                   Local_Count     => Sub.Frame_Words,
                                   Temporary_Count => Sub.Temporary_Words);
            end if;

            for I in 1 .. Sub.Transfers.Last_Index loop

               if Transfers.Has_Location (Sub.Transfers (I)) then
                  declare
                     Line   : constant Positive :=
                                Transfers.Get_Line (Sub.Transfers (I));
                  begin
                     Target.Set_Location
                       (File_Assembly_Type'Class (File), Line, 1);
                  end;
               end if;

               while Has_Element (Directive)
                 and then Element (Directive).Index <= I
               loop
                  File.Put_Line
                    (Ada.Strings.Unbounded.To_String
                       (Element (Directive).Value));
                  Next (Directive);
               end loop;

               if I = Sub.Executable_Segment.Last_Index
                 and then Unit.Last_Label (Executable)
                 /= Tagatha.Labels.No_Label
               then
                  Target.Label (File_Assembly_Type'Class (File),
                                Unit.Last_Label (Executable));
               end if;

               Target.Encode (File_Assembly_Type'Class (File),
                                 Sub.Transfers (I));
            end loop;

            while Has_Element (Directive) loop
               File.Put_Line
                 (Ada.Strings.Unbounded.To_String
                    (Element (Directive).Value));
               Next (Directive);
            end loop;

            if Sub.Has_Frame then
               Target.End_Frame (File_Assembly_Type'Class (File),
                                 Sub.Argument_Words,
                                 Sub.Frame_Words);
            end if;

            Target.Finish (File_Assembly_Type'Class (File));
         end;
      end loop;

      Target.Segment (File_Assembly_Type'Class (File), Read_Only);

      for Sub of Unit.Subprograms loop

         for Datum of Sub.Read_Only_Segment loop

            if Datum.Label /= Tagatha.Labels.No_Label then
               Target.Label (File_Assembly_Type'Class (File),
                             Datum.Label);
            end if;

            case Datum.Data_Type is
               when Integer_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Integer_Constant
                                 (Datum.Integer_Value));
               when Floating_Point_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Floating_Point_Constant
                                 (Datum.Floating_Point_Value));
               when Label_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Label_Constant
                                 (Datum.Label_Value));
               when String_Data =>
                  null;
            end case;
         end loop;
      end loop;

      Target.Segment (File_Assembly_Type'Class (File), Read_Write);

      for Sub of Unit.Subprograms loop

         for Datum of Sub.Read_Write_Segment loop

            if Datum.Label /= Tagatha.Labels.No_Label then
               Target.Label (File_Assembly_Type'Class (File),
                             Datum.Label);
            end if;

            case Datum.Data_Type is
               when Integer_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Integer_Constant
                                 (Datum.Integer_Value));
               when Floating_Point_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Floating_Point_Constant
                                 (Datum.Floating_Point_Value));
               when Label_Data =>
                  Target.Data (File_Assembly_Type'Class (File),
                               Tagatha.Constants.Label_Constant
                                 (Datum.Label_Value));
               when String_Data =>
                  null;
            end case;
         end loop;
      end loop;

      Target.File_Postamble (File_Assembly_Type'Class (File));
      Close (File);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Unit           : Tagatha_Unit;
      Target_Name    : String;
      Directory_Path : String)
   is
      Target : Tagatha.Code.Translator'Class :=
                 Tagatha.Code.Get_Translator (Target_Name);
   begin
      Allocate_Registers (Unit, Target);
      Write (Unit, Target, Directory_Path);
   end Write;

end Tagatha.Units;
