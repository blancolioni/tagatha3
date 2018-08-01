private with Ada.Strings.Unbounded;

with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Temporaries;

package Tagatha.Transfers is

   type Transfer is private;

   type Transfer_Operand is private;

   No_Operand : constant Transfer_Operand;

   function Is_Null_Operand
     (Operand : Transfer_Operand)
      return Boolean;

   function Is_Condition_Operand
     (Operand : Transfer_Operand)
      return Boolean;

   type Array_Of_Transfers is array (Positive range <>) of Transfer;

   function No_Transfers return Array_Of_Transfers;

   function To_Temporary (Src_1, Src_2 : Transfer_Operand;
                          Op           : Tagatha_Operator;
                          Dst          : Temporaries.Temporary)
                         return Transfer;

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                             return Transfer_Operand;

   function Argument_Operand
     (Arg_Index     : Argument_Offset;
      Indirect      : Boolean := False)
      return Transfer_Operand;

   function Local_Operand
     (Frame_Index   : Local_Offset;
      Indirect      : Boolean := False)
      return Transfer_Operand;

   function Result_Operand return Transfer_Operand;
   function Return_Operand return Transfer_Operand;
   function Iterator_New_Operand return Transfer_Operand;
   function Iterator_Copy_Operand return Transfer_Operand;

   function External_Operand
     (Name      : String;
      Immediate : Boolean;
      Predec    : Boolean := False;
      Postinc   : Boolean := False)
      return Transfer_Operand;

   function Text_Operand
     (Text      : String)
      return Transfer_Operand;

   function Shelf_Operand
     (Shelf_Name : String)
      return Transfer_Operand;

   function Label_Operand (Label         : Tagatha.Labels.Tagatha_Label)
                          return Transfer_Operand;

   function Temporary_Operand
     (Temp       : Tagatha.Temporaries.Temporary;
      Indirect   : Boolean := False)
      return Transfer_Operand;

   function Stack_Operand return Transfer_Operand;

   function Condition_Operand return Transfer_Operand;

   procedure Set_Size (Item : in out Transfer_Operand;
                       Size : in     Tagatha_Size);

   procedure Set_Size (Item : in out Transfer;
                       Size : in     Tagatha_Size);

   function Has_Size (Item : in Transfer_Operand) return Boolean;
   function Get_Size (Item : in Transfer_Operand) return Tagatha_Size;

   function Simple_Transfer (From          : Transfer_Operand;
                             To            : Transfer_Operand;
                             To_Address    : Boolean := False)
                            return Transfer;

   function Operation_Transfer (Src_1         : Transfer_Operand;
                                Src_2         : Transfer_Operand;
                                Op            : Tagatha_Operator;
                                To            : Transfer_Operand)
                               return Transfer;

   function Control_Transfer (Condition   : Tagatha_Condition;
                              Destination : Tagatha.Labels.Tagatha_Label)
                              return Transfer
     with Pre => Tagatha.Labels.Has_Label (Destination);

   function Native_Transfer (Name              : String;
                             Changed_Registers : String)
                             return Transfer;

   function Call (Destination : Tagatha.Labels.Tagatha_Label;
      Argument_Count : Natural)
                  return Transfer;

   function Reserve_Stack (Frame_Size : Natural) return Transfer;
   function Restore_Stack (Frame_Size : Natural) return Transfer;

   procedure Set_Label (T       : in out Transfer;
                        Label   : in     Tagatha.Labels.Tagatha_Label);

   function Get_Label (T : Transfer)
                      return Tagatha.Labels.Tagatha_Label;

   procedure Clear_Label
     (T       : in out Transfer);

   function Is_Frame_Reservation (T : Transfer) return Boolean;
   function Get_Reservation (T : Transfer) return Integer;

   function Is_Control (T : Transfer) return Boolean;
   function Is_Call    (T : Transfer) return Boolean;
   function Get_Condition (T : Transfer) return Tagatha_Condition;
   function Get_Destination (T : Transfer)
                            return Tagatha.Labels.Tagatha_Label;
   function Get_Argument_Count (T : Transfer) return Natural;

   function Same_Operand (Left, Right : Transfer_Operand) return Boolean;

   function Show (Item : Transfer) return String;
   function Show (Item : Transfer_Operand) return String;

   function Is_Constant (Item : Transfer_Operand) return Boolean;
   function Is_Constant_Zero (Item : Transfer_Operand) return Boolean;
   function Is_Argument (Item : Transfer_Operand) return Boolean;
   function Is_Local    (Item : Transfer_Operand) return Boolean;
   function Is_Result   (Item : Transfer_Operand) return Boolean;
   function Is_Return   (Item : Transfer_Operand) return Boolean;
   function Is_Stack    (Item : Transfer_Operand) return Boolean;
   function Is_External (Item : Transfer_Operand) return Boolean;
   function Is_Immediate (Item : Transfer_Operand) return Boolean;
   function Is_Dereferenced (Item : Transfer_Operand) return Boolean;
   function Is_Indirect (Item : Transfer_Operand) return Boolean;
   function Has_Predecrement (Item : Transfer_Operand) return Boolean;
   function Has_Postincrement (Item : Transfer_Operand) return Boolean;

   function Is_Shelf     (Item : Transfer_Operand) return Boolean;
   function Get_Shelf_Name (Item : Transfer_Operand) return String;

   function Is_Text     (Item : Transfer_Operand) return Boolean;
   function Get_Text    (Item : Transfer_Operand) return String;

   function Is_Temporary (Item : Transfer_Operand) return Boolean;
   function Get_Temporary
     (Item : Transfer_Operand)
      return Tagatha.Temporaries.Temporary;

   function Is_Iterator_New (Item : Transfer_Operand) return Boolean;
   function Is_Iterator_Copy (Item : Transfer_Operand) return Boolean;

   function External_Name (Item : Transfer_Operand) return String;

   function Has_Slice (Item : Transfer_Operand) return Boolean;
   function Slice_Fits (Item : Transfer_Operand;
                        Size : Tagatha_Size)
                       return Boolean;
   function Get_Slice_Octet_Offset (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Bit_Offset  (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Octet_Length (Item : Transfer_Operand)
                                  return Tagatha_Integer;
   function Get_Slice_Bit_Length  (Item : Transfer_Operand)
                                  return Tagatha_Integer;

   function Get_Slice_Mask (Item : Transfer_Operand) return Tagatha_Integer;

   function Slice (Item   : Transfer_Operand;
                   Offset : Natural;
                   Size   : Tagatha_Size)
                  return Transfer_Operand;

   function Get_Value (Item : Transfer_Operand)
                      return Tagatha.Constants.Tagatha_Constant;
   function Get_Arg_Offset (Item : Transfer_Operand) return Argument_Offset;
   function Get_Local_Offset (Item : Transfer_Operand) return Local_Offset;

   function Is_Simple    (Item : Transfer) return Boolean;
   function Is_Native    (Item : Transfer) return Boolean;
   function Has_Operator (Item : Transfer) return Boolean;

   function Get_Native_Text (Item : Transfer) return String;

   function Get_Destination (Item : Transfer) return Transfer_Operand;
   function Get_Source (Item : Transfer) return Transfer_Operand;
   function Get_Source_1 (Item : Transfer) return Transfer_Operand;
   function Get_Source_2 (Item : Transfer) return Transfer_Operand;
   function Get_Operator (Item : Transfer) return Tagatha_Operator;

   function Has_Location (Item : Transfer) return Boolean;
   function Get_Line (Item : Transfer) return Positive
     with Pre => Has_Location (Item);
   function Get_Column (Item : Transfer) return Positive
     with Pre => Has_Location (Item);

   procedure Reference_Temporaries
     (Item    : in out Transfer;
      Address : Positive);

   type Register_Allocation is
      record
         Start, Finish : Natural := 0;
      end record;

   type Register_Allocation_Array is
     array (Positive range <>) of Register_Allocation;

   procedure Assign_Registers
     (Item : in out Transfer;
      Rs   : in out Register_Allocation_Array;
      Last : in out Natural);

   procedure Set_Location
     (Item   : in out Transfer;
      Line   : Positive;
      Column : Positive);

private

   type Transfer_Operand_Type is
     (T_No_Operand, T_Stack, T_Temporary,
      T_Local, T_Argument, T_Result, T_Return,
      T_Immediate, T_External,
      T_Iterator,
      T_Condition, T_Text, T_Shelf);

   type Bit_Slice is
      record
         First_Bit : Tagatha_Integer;
         Last_Bit  : Tagatha_Integer;
      end record;

   type Source_Modification is
      record
         Have_Slice   : Boolean;
         Have_Size    : Boolean;
         Dereferenced : Boolean;
         Indirect     : Boolean;
         Slice        : Bit_Slice;
         Size         : Tagatha_Size;
      end record;

   No_Modification : constant Source_Modification :=
                       (False, False, False, False,
                        (0, 0),
                        Default_Integer_Size);

   type Transfer_Operand (Op : Transfer_Operand_Type := T_No_Operand) is
      record
         Modifiers     : Source_Modification;
         case Op is
            when T_No_Operand =>
               null;
            when T_Stack =>
               null;
            when T_Condition =>
               null;
            when T_Temporary =>
               Temp       : Tagatha.Temporaries.Temporary;
            when T_Local =>
               Loc_Offset : Local_Offset;
            when T_Argument =>
               Arg_Offset : Argument_Offset;
            when T_Result | T_Return =>
               null;
            when T_Immediate =>
               Value      : Tagatha.Constants.Tagatha_Constant;
            when T_Iterator =>
               New_Iterator  : Boolean;
               Copy_Iterator : Boolean;
            when T_External =>
               External_Name : Ada.Strings.Unbounded.Unbounded_String;
               External_Imm  : Boolean;
               External_Predec : Boolean;
               External_Postinc : Boolean;
            when T_Text =>
               Text             : Ada.Strings.Unbounded.Unbounded_String;
            when T_Shelf =>
               Shelf_Name : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   No_Operand : constant Transfer_Operand :=
                  (T_No_Operand, No_Modification);

   function Shelf_Operand
     (Shelf_Name : String)
      return Transfer_Operand
   is (T_Shelf, No_Modification,
       Ada.Strings.Unbounded.To_Unbounded_String (Shelf_Name));

   function Is_Shelf     (Item : Transfer_Operand) return Boolean
   is (Item.Op = T_Shelf);

   function Get_Shelf_Name (Item : Transfer_Operand) return String
   is (Ada.Strings.Unbounded.To_String (Item.Shelf_Name));

   function Has_Predecrement (Item : Transfer_Operand) return Boolean
   is (Item.Op = T_External and then Item.External_Predec);

   function Has_Postincrement (Item : Transfer_Operand) return Boolean
   is (Item.Op = T_External and then Item.External_Postinc);

   function Is_Iterator_New (Item : Transfer_Operand) return Boolean
   is (Item.Op = T_Iterator and then Item.New_Iterator);

   function Is_Iterator_Copy (Item : Transfer_Operand) return Boolean
   is (Item.Op = T_Iterator and then Item.Copy_Iterator);

   type Array_Of_Operands is array (Positive range <>) of Transfer_Operand;

   type Transfer_Type is
     (T_Control,
      T_Native,
      T_Data,
      T_Change_Stack);

   type Transfer is
      record
         Trans             : Transfer_Type;
         Reserve           : Integer;
         Label             : Tagatha.Labels.Tagatha_Label;
         Condition         : Tagatha_Condition;
         Destination       : Tagatha.Labels.Tagatha_Label;
         Argument_Count    : Natural;
         Call              : Boolean;
         Self              : Boolean;
         Native            : Ada.Strings.Unbounded.Unbounded_String;
         Changed_Registers : Ada.Strings.Unbounded.Unbounded_String;
         Line              : Natural := 0;
         Column            : Natural := 0;
         Src_1             : Transfer_Operand;
         Src_2             : Transfer_Operand;
         Dst               : Transfer_Operand;
         Op                : Tagatha_Operator;
         To_Address        : Boolean;
      end record;

   function Is_Dereferenced (Item : Transfer_Operand) return Boolean
   is (Item.Modifiers.Dereferenced
       and then not Item.Modifiers.Indirect);

   function Is_Indirect (Item : Transfer_Operand) return Boolean
   is (Item.Modifiers.Indirect
       and then not Item.Modifiers.Dereferenced);

   function Get_Argument_Count (T : Transfer) return Natural
   is (T.Argument_Count);

end Tagatha.Transfers;
