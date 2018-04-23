with Ada.Strings.Unbounded;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with WL.String_Maps;

private with Tagatha.Commands.Command_Vectors;
private with Tagatha.Transfers.Transfer_Vectors;

private with Tagatha.Labels;

with Tagatha.Commands;
with Tagatha.Operands;

package Tagatha.Units is

   --  note: all stack offsets are in _words_

   type Tagatha_Unit is tagged private;

   type Tagatha_Unit_Access is access all Tagatha_Unit'Class;

   procedure Create_Unit (New_Unit       : in out Tagatha_Unit;
                          Name           : in     String;
                          Source_File    : in     String);

   procedure Finish_Unit (Unit : in out Tagatha_Unit);

   function Unit_Name (Unit : Tagatha_Unit'Class) return String;
   function Unit_Label_Name (Unit : Tagatha_Unit'Class) return String;

   procedure Source_Position
     (Unit         : in out Tagatha_Unit;
      Line, Column : Positive);

   procedure Begin_Routine
     (Unit           : in out Tagatha_Unit;
      Name           : in     String;
      Argument_Words : in     Natural;
      Frame_Words    : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean);

   procedure End_Routine
     (Unit : in out Tagatha_Unit);

   procedure Optimise (Unit : in out Tagatha_Unit);

   procedure Write (Unit           : Tagatha_Unit;
                    Target_Name    : String;
                    Directory_Path : String);

   procedure Segment (Unit  : in out Tagatha_Unit;
                      Seg   : in     Tagatha_Segment);

   procedure Label (Unit     : in out Tagatha_Unit;
                    Name     : in     String;
                    Export   : in     Boolean := False);

   function Next_Label (Unit   : in out Tagatha_Unit) return Positive;

   procedure Label (Unit   : in out Tagatha_Unit;
                    Index  : in     Positive);

   procedure Command (Unit    : in out Tagatha_Unit;
                      Command : in     Tagatha.Commands.Tagatha_Command);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Data
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size     := Default_Integer_Size);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer_Array;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point_Array);

   procedure Data (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Floating_Point);

   procedure Ascii_String (Unit  : in out Tagatha_Unit;
                           Value : in     String);

   procedure Asciz_String (Unit  : in out Tagatha_Unit;
                           Value : in     String);

   procedure Directive (Unit : in out Tagatha_Unit;
                        Value : String;
                        Address : Integer := -1);

   procedure Push (Unit    : in out Tagatha_Unit;
                   Value   : in     Tagatha_Integer;
                   Size    : in     Tagatha_Size     := Default_Integer_Size);

   procedure Push (Unit  : in out Tagatha_Unit;
                   Value : in     Tagatha_Floating_Point);

   procedure Push_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : in     String;
      Size       : in     Tagatha_Size  := Default_Integer_Size;
      External   : in     Boolean       := False);

   procedure Push_Text
     (Unit : in out Tagatha_Unit;
      Text : String);

   procedure Push_Local (Unit    : in out Tagatha_Unit;
                         Offset  : in     Local_Offset;
                         Size    : Tagatha_Size := Default_Integer_Size);

   procedure Push_Argument (Unit    : in out Tagatha_Unit;
                            Offset  : in     Argument_Offset;
                            Size    : Tagatha_Size := Default_Integer_Size);

   procedure Push_Result
     (Unit    : in out Tagatha_Unit;
      Size    : in     Tagatha_Size := Default_Integer_Size);

   --  procedure Push_Label_Address (Unit       : in out Tagatha_Unit;
   --                                Label_Name : String);

   --  procedure Push_Local_Address (Unit    : in out Tagatha_Unit;
   --                                Offset  : in     Local_Offset);

   --  procedure Push_Argument_Address (Unit    : in out Tagatha_Unit;
   --                                   Offset  : in     Argument_Offset);

   procedure Pop_Label
     (Unit       : in out Tagatha_Unit;
      Label_Name : String;
      Size       : Tagatha_Size := Default_Integer_Size;
      External   : Boolean      := False);

   procedure Pop_Local (Unit    : in out Tagatha_Unit;
                        Offset  : in     Local_Offset;
                        Size    : in     Tagatha_Size :=
                          Default_Integer_Size);

   procedure Pop_Argument
     (Unit    : in out Tagatha_Unit;
      Offset  : in     Argument_Offset;
      Size    : in     Tagatha_Size := Default_Integer_Size);

   procedure Pop_Result
     (Unit    : in out Tagatha_Unit;
      Size    : in     Tagatha_Size := Default_Integer_Size);

   procedure Dereference (Unit : in out Tagatha_Unit;
                          Size : in     Tagatha_Size := Default_Integer_Size);

   procedure Store (Unit : in out Tagatha_Unit;
                    Size : Tagatha_Size := Default_Size);
   --  Top two stack values contain an address and a value, respectively.
   --  Store the value at the address

   procedure Operate (Unit   : in out Tagatha_Unit;
                      Op     : Tagatha_Operator;
                      Size   : Tagatha_Size       := Default_Integer_Size);

   procedure Call (Unit   : in out Tagatha_Unit;
                   Target : in     String);

   procedure Indirect_Call (Unit   : in out Tagatha_Unit);
   --  like call, but target address is on top of the stack

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     String);

   procedure Jump (Unit      : in out Tagatha_Unit;
                   Target    : in     Integer;
                   Condition : in     Tagatha_Condition := C_Always);

   procedure Loop_Around (Unit         : in out Tagatha_Unit;
                          Label_Name   : in     String;
                          Loop_Count   : in     Local_Offset;
                          Loop_Index   : in     Local_Offset);

   function External_Name (Unit : Tagatha_Unit) return String;
   function File_System_Name (Unit : Tagatha_Unit) return String;

   procedure Drop (Unit      : in out Tagatha_Unit;
                   Size      : in     Tagatha_Size := Default_Size);

   procedure Pop_Operand (Unit      : in out Tagatha_Unit;
                          Op        : in     Operands.Tagatha_Operand;
                          Size      : in     Tagatha_Size);

   procedure Push_Operand (Unit      : in out Tagatha_Unit;
                           Op        : in     Operands.Tagatha_Operand;
                           Size      : in     Tagatha_Size);

   procedure Duplicate (Unit : in out Tagatha_Unit);

   procedure Swap (Unit : in out Tagatha_Unit);

   procedure Save_Top (Unit : in out Tagatha_Unit);
   procedure Restore_Top (Unit : in out Tagatha_Unit);

   procedure Start_Copy
     (Unit      : in out Tagatha_Unit);
   --  Start a bulk copy operation.  The address on top of the stack
   --  is the address of the beginning of the block.

   procedure Copy_To
     (Unit : in out Tagatha_Unit;
      Size : Tagatha_Size := Default_Size);
   --  Copy an item of the given size from stack top to the current
   --  bulk copy address.  Increment the address.

   procedure Copy_From
     (Unit : in out Tagatha_Unit;
      Size : Tagatha_Size := Default_Size);
   --  Push an item of the given size from the current bulk copy address.
   --  Increment the address.

   procedure End_Copy
     (Unit : in out Tagatha_Unit);

   procedure Native_Operation
     (Unit               : in out Tagatha_Unit;
      Name               : String;
      Input_Stack_Words  : Natural := 0;
      Output_Stack_Words : Natural := 0;
      Changed_Registers  : String := "");
   --  An operation which is understood by the target architecture is created.
   --  Input_Stack_Words holds the number of words which the
   --  operation pops off the stack before executing.
   --  Output_Stack_Words holds the number of words pushed to the stack
   --  after execution.
   --  Changed_Registers should be a comma-separated list
   --  of registers that can be changed by this operation
   --  If empty, no (relevant) registers are changed

   procedure Set_Property
     (Unit  : in out Tagatha_Unit;
      Name  : String;
      Value : String);

   procedure Clear_Property
     (Unit  : in out Tagatha_Unit;
      Name  : String);

   function Get_Property
     (Unit    : Tagatha_Unit;
      Name    : String;
      Default : String := "")
      return String;

private

   type Tagatha_Data_Type is
     (Integer_Data, Floating_Point_Data,
      String_Data, Label_Data);

   type Tagatha_Data (Data_Type : Tagatha_Data_Type := Integer_Data) is
      record
         Label : Tagatha.Labels.Tagatha_Label;
         Size : Tagatha_Size;
         case Data_Type is
            when Integer_Data =>
               Integer_Value        : Tagatha_Integer;
            when Floating_Point_Data =>
               Floating_Point_Value : Tagatha_Floating_Point;
            when Label_Data =>
               Label_Value          : Tagatha.Labels.Tagatha_Label;
            when String_Data =>
               String_Value         : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Data_Vector is
     new Ada.Containers.Vectors (Positive, Tagatha_Data);

   type Segment_Length_Array is array (Tagatha_Segment) of Natural;
   type Last_Label_Array is
     array (Tagatha_Segment) of Tagatha.Labels.Tagatha_Label;

   type Directive_Record is
      record
         Segment : Tagatha_Segment;
         Index   : Natural;
         Value   : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package List_Of_Directives is
     new Ada.Containers.Doubly_Linked_Lists (Directive_Record);

   type Source_Reference is
      record
         Line, Column : Positive;
         Segment      : Tagatha_Segment;
         Start        : Natural;
      end record;

   package List_Of_Source_References is
      new Ada.Containers.Doubly_Linked_Lists (Source_Reference);

   type Tagatha_Subprogram_Record is
      record
         Name               : Ada.Strings.Unbounded.Unbounded_String;
         Current_Segment    : Tagatha_Segment        := Executable;
         Next_Address       : Segment_Length_Array   := (others => 1);
         Argument_Words     : Natural;
         Frame_Words        : Natural;
         Result_Words       : Natural;
         Last_Label         : Tagatha.Labels.Tagatha_Label;
         Last_Line          : Natural := 0;
         Last_Column        : Natural := 0;
         Global             : Boolean := True;
         Executable_Segment : Tagatha.Commands.Command_Vectors.Vector;
         Read_Only_Segment  : Data_Vector.Vector;
         Read_Write_Segment : Data_Vector.Vector;
         Directives         : List_Of_Directives.List;
         Transfers          : Tagatha.Transfers.Transfer_Vectors.Vector;
      end record;

   type Tagatha_Subprogram is access Tagatha_Subprogram_Record;

   function Subprogram_Name
     (Subprogram : Tagatha_Subprogram)
      return String
   is (Ada.Strings.Unbounded.To_String (Subprogram.Name));

   package List_Of_Subprograms is
     new Ada.Containers.Doubly_Linked_Lists
       (Tagatha_Subprogram);

   package Property_Maps is
      new WL.String_Maps (String);

   type Tagatha_Unit is tagged
      record
         Name               : Ada.Strings.Unbounded.Unbounded_String;
         Source_File        : Ada.Strings.Unbounded.Unbounded_String;
         Current_Segment    : Tagatha_Segment        := Executable;
         Directives         : List_Of_Directives.List;
         Labels             : Tagatha.Labels.Tagatha_Label_List;
         Last_Label         : Last_Label_Array;
         Next_Label         : Positive := 1;
         Next_String        : Positive := 1;
         Subprograms        : List_Of_Subprograms.List;
         Current_Sub        : Tagatha_Subprogram;
         Properties         : Property_Maps.Map;
      end record;

   function Get_Property
     (Unit    : Tagatha_Unit;
      Name    : String;
      Default : String := "")
      return String
   is (if Unit.Properties.Contains (Name)
       then Unit.Properties.Element (Name)
       else Default);

   function Unit_Name (Unit : Tagatha_Unit'Class) return String
   is (Ada.Strings.Unbounded.To_String (Unit.Name));

end Tagatha.Units;
