with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Tagatha.Code;
with Tagatha.Commands.Registry;
with Tagatha.Constants;
with Tagatha.File_Assembly;
with Tagatha.Transfers;
with Tagatha.Transfers.Optimiser;

with Tagatha.Registry;

package body Tagatha.Units is

   Trace_Commands : constant Boolean := False;

   procedure Allocate_Registers
     (Unit : Tagatha_Unit;
      Register_Count : Positive);

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

   ------------------------
   -- Allocate_Registers --
   ------------------------

   procedure Allocate_Registers
     (Unit : Tagatha_Unit;
      Register_Count : Positive)
   is
      Allocation   : Transfers.Register_Allocation_Array (1 .. Register_Count);
      Base_Address : Natural := 0;
   begin
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
         for Offset in 1 .. Sub.Transfers.Last_Index loop
            Tagatha.Transfers.Assign_Registers
              (Sub.Transfers (Offset), Allocation);
         end loop;
      end loop;
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
      if To_Unit.Current_Sub.Last_Line > 0 then
         Commands.Set_Source_Reference
           (Command, To_Unit.Current_Sub.Last_Line,
            To_Unit.Current_Sub.Last_Column);
      end if;

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
        ((String_Data, Unit.Last_Label (Unit.Current_Segment), Size_8,
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

   -------------------
   -- Begin_Routine --
   -------------------

   procedure Begin_Routine
     (Unit           : in out Tagatha_Unit;
      Name           : in     String;
      Argument_Words : in     Natural;
      Frame_Words    : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean)
   is
      use Ada.Strings.Unbounded;
   begin
      Unit.Current_Sub := new Tagatha_Subprogram_Record;
      Unit.Subprograms.Append (Unit.Current_Sub);
      Unit.Current_Sub.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Unit.Current_Sub.Argument_Words := Argument_Words;
      Unit.Current_Sub.Frame_Words := Frame_Words;
      Unit.Current_Sub.Result_Words := Result_Words;
      Unit.Current_Sub.Global := Global;
   end Begin_Routine;

   ----------
   -- Call --
   ----------

   procedure Call (Unit   : in out Tagatha_Unit;
                   Target : in     String)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Target, Import => True);
      Append (Unit, Commands.Call (Label));
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
      New_Value : constant Tagatha_Data :=
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
      New_Value : Tagatha_Data;
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
      New_Value : constant Tagatha_Data :=
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

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference (Unit : in out Tagatha_Unit;
                          Size : in     Tagatha_Size := Default_Integer_Size)
   is
   begin
      Operate (Unit, Op_Dereference, Size);
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
                      (Op_Nop, False, Default_Size);
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
     (Unit   : in out Tagatha_Unit)
   is
   begin
      Append (Unit, Commands.Indirect_Call);
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

   procedure Operate (Unit   : in out Tagatha_Unit;
                      Op     : Tagatha_Operator;
                      Size   : Tagatha_Size       := Default_Integer_Size)
   is
   begin
      Append (Unit, Tagatha.Commands.Operate (Op, False, Size));
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

   ------------------
   -- Pop_Argument --
   ------------------

   procedure Pop_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Argument_Operand (Offset), Size));
   end Pop_Argument;

   ---------------
   -- Pop_Label --
   ---------------

   procedure Pop_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : String;
      Size       : Tagatha_Size := Default_Integer_Size;
      External   : Boolean      := False)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => External);
      Append (Unit,
              Commands.Pop (Operands.Label_Operand (Label), Size));
   end Pop_Label;

   ---------------
   -- Pop_Local --
   ---------------

   procedure Pop_Local
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Local_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Local_Operand (Offset), Size));
   end Pop_Local;

   -----------------
   -- Pop_Operand --
   -----------------

   procedure Pop_Operand (Unit      : in out Tagatha_Unit;
                          Op        : in     Tagatha.Operands.Tagatha_Operand;
                          Size      : in     Tagatha_Size)
   is
   begin
      Append (Unit, Commands.Pop (Op, Size));
   end Pop_Operand;

   ------------------
   -- Pop_Register --
   ------------------

   procedure Pop_Register
     (Unit : in out Tagatha_Unit;
      Name : in     String)
   is
   begin
      Pop_Operand (Unit, Operands.Register_Operand (Name), Default_Size);
   end Pop_Register;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Pop_Result
     (Unit       : in out Tagatha_Unit;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Pop (Operands.Result_Operand, Size));
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
              Commands.Push (Operands.Constant_Operand (Value), Size));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point)
   is
   begin
      Append (Unit,
        Commands.Push (Operands.Constant_Operand (Value),
          Default_Size));
   end Push;

   -------------------
   -- Push_Argument --
   -------------------

   procedure Push_Argument
     (Unit       : in out Tagatha_Unit;
      Offset     : in     Argument_Offset;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Argument_Operand (Offset), Size));
   end Push_Argument;

   ---------------------------
   -- Push_Argument_Address --
   ---------------------------

   --  procedure Push_Argument_Address
   --    (Unit       : in out Tagatha_Unit;
   --     Offset     : in     Argument_Offset)
   --  is
   --  begin
   --     Append (Unit,
   --             Commands.Push (Operands.Argument_Operand (Offset), Size));
   --  end Push_Argument_Address;

   ----------------
   -- Push_Label --
   ----------------

   procedure Push_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size  := Default_Integer_Size;
      External   : in     Boolean       := False)
   is
      Label : Tagatha.Labels.Tagatha_Label;
   begin
      Tagatha.Labels.Reference_Label (Unit.Labels, Label,
                                      Label_Name, Import => External);
      Append (Unit,
              Commands.Push (Operands.Label_Operand (Label), Size));
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
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Local_Operand (Offset), Size));
   end Push_Local;

   ------------------
   -- Push_Operand --
   ------------------

   procedure Push_Operand (Unit      : in out Tagatha_Unit;
                           Op        : in     Operands.Tagatha_Operand;
                           Size      : in     Tagatha_Size)
   is
   begin
      Append (Unit, Commands.Push (Op, Size));
   end Push_Operand;

   -------------------
   -- Push_Register --
   -------------------

   procedure Push_Register
     (Unit : in out Tagatha_Unit;
      Name : in     String)
   is
   begin
      Push_Operand (Unit, Operands.Register_Operand (Name), Default_Size);
   end Push_Register;

   ----------------
   -- Pop_Result --
   ----------------

   procedure Push_Result
     (Unit       : in out Tagatha_Unit;
      Size       : in     Tagatha_Size  := Default_Integer_Size)
   is
   begin
      Append (Unit,
              Commands.Push (Operands.Result_Operand, Size));
   end Push_Result;

   ---------------
   -- Push_Text --
   ---------------

   procedure Push_Text
     (Unit : in out Tagatha_Unit;
      Text : String)
   is
   begin
      Push_Operand (Unit, Operands.Text_Operand (Text), Default_Size);
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

   ---------------------
   -- Source_Position --
   ---------------------

   procedure Source_Position
     (Unit         : in out Tagatha_Unit;
      Line, Column : Positive)
   is
   begin
      if Unit.Current_Sub /= null then
         Unit.Current_Sub.Last_Line := Line;
         Unit.Current_Sub.Last_Column := Column;
      end if;
   end Source_Position;

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
      Current_Line : Natural := 0;
      Current_Column : Natural := 0;
   begin
      Open (File, File_Path);
      Target.File_Preamble (File_Assembly_Type'Class (File),
                            Ada.Strings.Unbounded.To_String
                              (Unit.Source_File));

      for Directive of Unit.Directives loop
         File.Put_Line (Ada.Strings.Unbounded.To_String (Directive.Value));
      end loop;

      for Sub of Unit.Subprograms loop

         if Sub.Transfers.Last_Index > 0
           and then Transfers.Has_Location (Sub.Transfers (1))
         then
            declare
               Line : constant Positive :=
                        Transfers.Get_Line (Sub.Transfers (1));
               Column : constant Positive :=
                          Transfers.Get_Column (Sub.Transfers (1));
            begin
               Target.Set_Location
                 (File_Assembly_Type'Class (File), Line, Column);
               Current_Line := Line;
               Current_Column := Column;
            end;
         end if;

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

            Target.Begin_Frame (File_Assembly_Type'Class (File),
                                Sub.Argument_Words,
                                Sub.Frame_Words);

            for I in 1 .. Sub.Transfers.Last_Index loop

               if Transfers.Has_Location (Sub.Transfers (I)) then
                  declare
                     Line   : constant Positive :=
                                Transfers.Get_Line (Sub.Transfers (I));
                     Column : constant Positive :=
                                Transfers.Get_Column (Sub.Transfers (I));
                  begin
                     if Line /= Current_Line
                       or else Column /= Current_Column
                     then
                        Target.Set_Location
                          (File_Assembly_Type'Class (File), Line, Column);
                        Current_Line := Line;
                        Current_Column := Column;
                     end if;
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

            Target.End_Frame (File_Assembly_Type'Class (File),
                              Sub.Argument_Words,
                              Sub.Frame_Words);
            Target.Finish (File_Assembly_Type'Class (File));
         end;
      end loop;

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
      Allocate_Registers (Unit, Target.General_Registers);
      Write (Unit, Target, Directory_Path);
   end Write;

end Tagatha.Units;
