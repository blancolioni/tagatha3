with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Tagatha.Transfers is

   function Show_Operator (Item : Tagatha_Operator) return String;

   ----------------------
   -- Argument_Operand --
   ----------------------

   function Argument_Operand
     (Arg_Index     : Argument_Offset;
      Indirect      : Boolean := False)
      return Transfer_Operand
   is
   begin
      return (T_Argument,
              (No_Modification with delta Indirect => Indirect),
              Arg_Index);
   end Argument_Operand;

   ----------------------
   -- Assign_Registers --
   ----------------------

   procedure Assign_Registers
     (Item : in out Transfer;
      Rs   : in out Register_Allocation_Array;
      Last : in out Natural)
   is

      procedure Assign (Op : in out Transfer_Operand);

      ------------
      -- Assign --
      ------------

      procedure Assign (Op : in out Transfer_Operand) is
         Assigned : Boolean := False;
      begin
         if Op.Op = T_Temporary
           and then Temporaries.Get_Register (Op.Temp) = 0
         then
            for I in Rs'Range loop
               declare
                  R : Register_Allocation renames Rs (I);
               begin
                  if R.Finish < Temporaries.First_Reference (Op.Temp) then
                     Temporaries.Assign_Register (Op.Temp, I);
                     R := (Temporaries.First_Reference (Op.Temp),
                           Temporaries.Last_Reference (Op.Temp));
                     Assigned := True;
                     Last := Natural'Max (Last, I);
                     exit;
                  end if;
               end;
            end loop;

            if not Assigned then
               Ada.Text_IO.Put_Line ("warning: register spill");
               Temporaries.Assign_Register (Op.Temp, Rs'Length + 1);
            end if;
         end if;
      end Assign;

   begin
      Assign (Item.Src_1);
      Assign (Item.Src_2);
      Assign (Item.Dst);
   end Assign_Registers;

   ----------
   -- Call --
   ----------

   function Call
     (Destination : Tagatha.Labels.Tagatha_Label;
      Argument_Count : Natural)
      return Transfer
   is
   begin
      return (Trans             => T_Control,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => C_Always,
              Destination       => Destination,
              Argument_Count    => Argument_Count,
              Self              => False,
              Call              => True,
              Native            => Ada.Strings.Unbounded.Null_Unbounded_String,
              Changed_Registers => Ada.Strings.Unbounded.Null_Unbounded_String,
              Line              => 0,
              Column            => 0,
              Src_1             => No_Operand,
              Src_2             => No_Operand,
              Dst               => No_Operand,
              To_Address        => False,
              Op                => Op_Nop);
   end Call;

   -----------------
   -- Clear_Label --
   -----------------

   procedure Clear_Label
     (T       : in out Transfer)
   is
   begin
      T.Label := Tagatha.Labels.No_Label;
   end Clear_Label;

   -----------------------
   -- Condition_Operand --
   -----------------------

   function Condition_Operand return Transfer_Operand is
   begin
      return (T_Condition, No_Modification);
   end Condition_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                              return Transfer_Operand
   is
   begin
      return (T_Immediate, No_Modification, Value);
   end Constant_Operand;

   ----------------------
   -- Control_Transfer --
   ----------------------

   function Control_Transfer
     (Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Tagatha_Label)
      return Transfer
   is
   begin
      return (Trans             => T_Control,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => Condition,
              Destination       => Destination,
              Argument_Count    => 0,
              Self              => False,
              Call              => False,
              Native            => Ada.Strings.Unbounded.Null_Unbounded_String,
              Changed_Registers => Ada.Strings.Unbounded.Null_Unbounded_String,
              Line              => 0,
              Column            => 0,
              Src_1             => No_Operand,
              Src_2             => No_Operand,
              Dst               => No_Operand,
              To_Address        => False,
              Op                => Op_Nop);
   end Control_Transfer;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Item : Transfer_Operand) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.External_Name);
   end External_Name;

   ----------------------
   -- External_Operand --
   ----------------------

   function External_Operand
     (Name      : String;
      Immediate : Boolean;
      Predec    : Boolean := False;
      Postinc   : Boolean := False)
      return Transfer_Operand
   is
   begin
      return (T_External, No_Modification,
              Ada.Strings.Unbounded.To_Unbounded_String (Name),
              Immediate, Predec, Postinc);
   end External_Operand;

   --------------------
   -- Get_Arg_Offset --
   --------------------

   function Get_Arg_Offset (Item : Transfer_Operand) return Argument_Offset is
   begin
      return Item.Arg_Offset;
   end Get_Arg_Offset;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Item : Transfer) return Positive is
   begin
      return Item.Column;
   end Get_Column;

   -------------------
   -- Get_Condition --
   -------------------

   function Get_Condition (T : Transfer) return Tagatha_Condition is
   begin
      return T.Condition;
   end Get_Condition;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (T : Transfer)
                            return Tagatha.Labels.Tagatha_Label
   is
   begin
      return T.Destination;
   end Get_Destination;

   ---------------------
   -- Get_Destination --
   ---------------------

   function Get_Destination (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Dst;
   end Get_Destination;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (T : Transfer)
                      return Tagatha.Labels.Tagatha_Label
   is
   begin
      return T.Label;
   end Get_Label;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Item : Transfer) return Positive is
   begin
      return Item.Line;
   end Get_Line;

   ----------------------
   -- Get_Local_Offset --
   ----------------------

   function Get_Local_Offset (Item : Transfer_Operand) return Local_Offset is
   begin
      return Item.Loc_Offset;
   end Get_Local_Offset;

   ---------------------
   -- Get_Native_Text --
   ---------------------

   function Get_Native_Text (Item : Transfer) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Native);
   end Get_Native_Text;

   ------------------
   -- Get_Operator --
   ------------------

   function Get_Operator (Item : Transfer) return Tagatha_Operator is
   begin
      return Item.Op;
   end Get_Operator;

   ---------------------
   -- Get_Reservation --
   ---------------------

   function Get_Reservation (T : Transfer) return Integer is
   begin
      return T.Reserve;
   end Get_Reservation;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Item : Transfer_Operand) return Tagatha_Size is
   begin
      return Item.Modifiers.Size;
   end Get_Size;

   --------------------------
   -- Get_Slice_Bit_Length --
   --------------------------

   function Get_Slice_Bit_Length  (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
   begin
      return Item.Modifiers.Slice.Last_Bit -
        Item.Modifiers.Slice.First_Bit + 1;
   end Get_Slice_Bit_Length;

   --------------------------
   -- Get_Slice_Bit_Offset --
   --------------------------

   function Get_Slice_Bit_Offset  (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
   begin
      if Item.Modifiers.Have_Slice then
         return Item.Modifiers.Slice.First_Bit;
      else
         return 0;
      end if;
   end Get_Slice_Bit_Offset;

   --------------------
   -- Get_Slice_Mask --
   --------------------

   function Get_Slice_Mask (Item : Transfer_Operand)
                           return Tagatha_Integer is
   begin
      if not Item.Modifiers.Have_Slice then
         return Tagatha_Integer'Last;
      else
         return (2**Integer (Get_Slice_Bit_Length (Item)) - 1) *
           Get_Slice_Bit_Offset (Item);
      end if;
   end Get_Slice_Mask;

   ----------------------------
   -- Get_Slice_Octet_Length --
   ----------------------------

   function Get_Slice_Octet_Length (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
      Bit_Length : Tagatha_Integer := Get_Slice_Bit_Length (Item);
   begin
      if Bit_Length mod 8 /= 0 then
         Bit_Length := Bit_Length + (8 - Bit_Length mod 8);
      end if;
      return Bit_Length;
   end Get_Slice_Octet_Length;

   ----------------------------
   -- Get_Slice_Octet_Offset --
   ----------------------------

   function Get_Slice_Octet_Offset (Item : Transfer_Operand)
                                  return Tagatha_Integer
   is
      Bit_Offset : constant Tagatha_Integer := Get_Slice_Bit_Offset (Item);
   begin
      return Bit_Offset - Bit_Offset mod 8;
   end Get_Slice_Octet_Offset;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : Transfer) return Transfer_Operand is
   begin
      return Get_Source_1 (Item);
   end Get_Source;

   ------------------
   -- Get_Source_1 --
   ------------------

   function Get_Source_1 (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Src_1;
   end Get_Source_1;

   ------------------
   -- Get_Source_2 --
   ------------------

   function Get_Source_2 (Item : Transfer) return Transfer_Operand is
   begin
      return Item.Src_2;
   end Get_Source_2;

   -------------------
   -- Get_Temporary --
   -------------------

   function Get_Temporary
     (Item : Transfer_Operand)
      return Tagatha.Temporaries.Temporary
   is
   begin
      return Item.Temp;
   end Get_Temporary;

   --------------
   -- Get_Text --
   --------------

   function Get_Text    (Item : Transfer_Operand) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Text);
   end Get_Text;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Transfer_Operand)
                      return Tagatha.Constants.Tagatha_Constant
   is
   begin
      return Item.Value;
   end Get_Value;

   ------------------
   -- Has_Location --
   ------------------

   function Has_Location (Item : Transfer) return Boolean is
   begin
      return Item.Line > 0 and then Item.Column > 0;
   end Has_Location;

   ------------------
   -- Has_Operator --
   ------------------

   function Has_Operator (Item : Transfer) return Boolean is
   begin
      return Item.Op /= Op_Nop;
   end Has_Operator;

   --------------
   -- Has_Size --
   --------------

   function Has_Size (Item : in Transfer_Operand) return Boolean is
   begin
      return Item.Modifiers.Have_Size;
   end Has_Size;

   ---------------
   -- Has_Slice --
   ---------------

   function Has_Slice (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Modifiers.Have_Slice;
   end Has_Slice;

   -----------------
   -- Is_Argument --
   -----------------

   function Is_Argument (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Argument;
   end Is_Argument;

   -------------
   -- Is_Call --
   -------------

   function Is_Call    (T : Transfer) return Boolean is
   begin
      return T.Trans = T_Control and then T.Call;
   end Is_Call;

   --------------------------
   -- Is_Condition_Operand --
   --------------------------

   function Is_Condition_Operand
     (Operand : Transfer_Operand)
      return Boolean
   is
   begin
      return Operand.Op = T_Condition;
   end Is_Condition_Operand;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Immediate;
   end Is_Constant;

   ----------------------
   -- Is_Constant_Zero --
   ----------------------

   function Is_Constant_Zero (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Immediate and then
        Tagatha.Constants.Is_Integer (Item.Value) and then
        Tagatha.Constants.Get_Integer (Item.Value) = 0;
   end Is_Constant_Zero;

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (T : Transfer) return Boolean is
   begin
      return T.Trans = T_Control and then not T.Call;
   end Is_Control;

   -----------------
   -- Is_External --
   -----------------

   function Is_External (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_External;
   end Is_External;

   --------------------------
   -- Is_Frame_Reservation --
   --------------------------

   function Is_Frame_Reservation (T : Transfer) return Boolean is
   begin
      return T.Trans = T_Change_Stack;
   end Is_Frame_Reservation;

   ------------------
   -- Is_Immediate --
   ------------------

   function Is_Immediate (Item : Transfer_Operand) return Boolean is
   begin
      return (Item.Op = T_External and then Item.External_Imm)
        or else Item.Op = T_Immediate;
   end Is_Immediate;

   --------------
   -- Is_Local --
   --------------

   function Is_Local    (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Local;
   end Is_Local;

   ---------------
   -- Is_Native --
   ---------------

   function Is_Native    (Item : Transfer) return Boolean is
   begin
      return Item.Trans = T_Native;
   end Is_Native;

   ---------------------
   -- Is_Null_Operand --
   ---------------------

   function Is_Null_Operand
     (Operand : Transfer_Operand)
      return Boolean
   is
   begin
      return Operand.Op = T_No_Operand;
   end Is_Null_Operand;

   ---------------
   -- Is_Result --
   ---------------

   function Is_Result   (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Result;
   end Is_Result;

   ---------------
   -- Is_Return --
   ---------------

   function Is_Return   (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Return;
   end Is_Return;

   ---------------
   -- Is_Simple --
   ---------------

   function Is_Simple    (Item : Transfer) return Boolean is
   begin
      return Item.Op = Op_Nop;
   end Is_Simple;

   --------------
   -- Is_Stack --
   --------------

   function Is_Stack    (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Stack;
   end Is_Stack;

   ------------------
   -- Is_Temporary --
   ------------------

   function Is_Temporary (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Temporary;
   end Is_Temporary;

   -------------
   -- Is_Text --
   -------------

   function Is_Text (Item : Transfer_Operand) return Boolean is
   begin
      return Item.Op = T_Text;
   end Is_Text;

   ---------------------------
   -- Iterator_Copy_Operand --
   ---------------------------

   function Iterator_Copy_Operand return Transfer_Operand is
   begin
      return (T_Iterator, No_Modification,
              New_Iterator => False, Copy_Iterator => True);
   end Iterator_Copy_Operand;

   --------------------------
   -- Iterator_New_Operand --
   --------------------------

   function Iterator_New_Operand return Transfer_Operand is
   begin
      return (T_Iterator, No_Modification,
              New_Iterator => True, Copy_Iterator => False);
   end Iterator_New_Operand;

   -------------------
   -- Label_Operand --
   -------------------

   function Label_Operand (Label         : Tagatha.Labels.Tagatha_Label)
                           return Transfer_Operand
   is
   begin
      return (T_Immediate, No_Modification,
              Constants.Label_Constant (Label));
   end Label_Operand;

   -------------------
   -- Local_Operand --
   -------------------

   function Local_Operand
     (Frame_Index   : Local_Offset;
      Indirect      : Boolean := False)
      return Transfer_Operand
   is
   begin
      return (T_Local,
              (No_Modification with delta Indirect => Indirect),
              Frame_Index);
   end Local_Operand;

   ---------------------
   -- Native_Transfer --
   ---------------------

   function Native_Transfer
     (Name              : String;
      Changed_Registers : String)
      return Transfer
   is
      use Ada.Strings.Unbounded;
   begin
      return (Trans             => T_Native,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => C_Always,
              Destination       => Tagatha.Labels.No_Label,
              Argument_Count    => 0,
              Self              => False,
              Call              => False,
              Native            => To_Unbounded_String (Name),
              Changed_Registers => To_Unbounded_String (Changed_Registers),
              Line              => 0,
              Column            => 0,
              Src_1             => No_Operand,
              Src_2             => No_Operand,
              Dst               => No_Operand,
              To_Address        => False,
              Op                => Op_Nop);
   end Native_Transfer;

   ------------------
   -- No_Transfers --
   ------------------

   function No_Transfers return Array_Of_Transfers is
   begin
      return Result : Array_Of_Transfers (1 .. 0) do
         null;
      end return;
   end No_Transfers;

   ------------------------
   -- Operation_Transfer --
   ------------------------

   function Operation_Transfer (Src_1         : Transfer_Operand;
                                Src_2         : Transfer_Operand;
                                Op            : Tagatha_Operator;
                                To            : Transfer_Operand)
                                return Transfer
   is
   begin
      return (Trans             => T_Data,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => C_Always,
              Destination       => Tagatha.Labels.No_Label,
              Self              => Same_Operand (Src_1, To),
              Argument_Count    => 0,
              Call              => False,
              Native            => Ada.Strings.Unbounded.Null_Unbounded_String,
              Changed_Registers => Ada.Strings.Unbounded.Null_Unbounded_String,
              Line              => 0,
              Column            => 0,
              Src_1             => Src_1,
              Src_2             => Src_2,
              Dst               => To,
              To_Address        => False,
              Op                => Op);
   end Operation_Transfer;

   --------------
   -- Optimise --
   --------------

--     function Optimise
--       (Transfers : Array_Of_Transfers)
--        return Array_Of_Transfers
--     is
--        Result : Array_Of_Transfers (Transfers'Range);
--        From_Index : Positive := Transfers'First;
--        To_Index   : Positive := Result'First;
--
--     begin
--        while From_Index <= Transfers'Last loop
--           Ada.Text_IO.Put_Line ("checking: "
--                                 & Show (Transfers (From_Index)));
--           if From_Index < Transfers'Last - 1
--             and then Transfers (From_Index).Op = Op_Not
--             and then Transfers (From_Index + 1).Op = Op_Test
--             and then Transfers (From_Index + 2).Trans = T_Control
--             and then Transfers (From_Index + 2).Condition /= C_Always
--           then
--              Ada.Text_IO.Put_Line ("  -- compressing conditional jump");
--              Result (To_Index) := Transfers (From_Index + 2);
--              Result (To_Index).Condition :=
--                Negate (Result (To_Index).Condition);
--              To_Index := To_Index + 1;
--              From_Index := From_Index + 3;
--           elsif Transfers (From_Index).Trans = T_Data
--             and then Transfers (From_Index).Op = Op_Nop
--             and then Same_Operand
--               (Transfers (From_Index).Src_1,
--                Transfers (From_Index).Dst)
--           then
--              Ada.Text_IO.Put_Line ("  -- skipping null operation");
--              From_Index := From_Index + 1;
--           else
--              Result (To_Index) := Transfers (From_Index);
--              To_Index := To_Index + 1;
--              From_Index := From_Index + 1;
--           end if;
--        end loop;
--        return Result (Result'First .. To_Index - 1);
--     end Optimise;

   -------------------------
   -- Reference_Temporary --
   -------------------------

   procedure Reference_Temporaries
     (Item : in out Transfer;
      Address : Positive)
   is
      procedure Ref (Op : in out Transfer_Operand);

      ---------
      -- Ref --
      ---------

      procedure Ref (Op : in out Transfer_Operand) is
      begin
         if Op.Op = T_Temporary then
            Temporaries.Record_Reference (Op.Temp, Address);
         end if;
      end Ref;

   begin
      Ref (Item.Src_1);
      Ref (Item.Src_2);
      Ref (Item.Dst);
   end Reference_Temporaries;

   -------------------
   -- Reserve_Stack --
   -------------------

   function Reserve_Stack (Frame_Size : Natural) return Transfer is
      Result : Transfer;
   begin
      Result.Trans   := T_Change_Stack;
      Result.Reserve := Frame_Size;
      return Result;
   end Reserve_Stack;

   -------------------
   -- Restore_Stack --
   -------------------

   function Restore_Stack (Frame_Size : Natural) return Transfer is
      Result : Transfer;
   begin
      Result.Trans   := T_Change_Stack;
      Result.Reserve := -Frame_Size;
      return Result;
   end Restore_Stack;

   --------------------
   -- Result_Operand --
   --------------------

   function Result_Operand return Transfer_Operand is
   begin
      return (T_Result, No_Modification);
   end Result_Operand;

   --------------------
   -- Return_Operand --
   --------------------

   function Return_Operand return Transfer_Operand is
   begin
      return (T_Return, No_Modification);
   end Return_Operand;

   ------------------
   -- Same_Operand --
   ------------------

   function Same_Operand (Left, Right : Transfer_Operand) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Tagatha.Constants.Tagatha_Constant;
      use type Tagatha.Temporaries.Temporary;
   begin
      if Left.Op /= Right.Op then
         return False;
      elsif Left.Modifiers /= Right.Modifiers then
         return False;
      else
         case Left.Op is
            when T_No_Operand =>
               return True;
            when T_Stack =>
               return True;
            when T_Condition =>
               return True;
            when T_Temporary =>
               return Left.Temp = Right.Temp;
            when T_Local =>
               return Left.Loc_Offset = Right.Loc_Offset;
            when T_Argument =>
               return Left.Arg_Offset = Right.Arg_Offset;
            when T_Result =>
               return True;
            when T_Return =>
               return True;
            when T_Immediate =>
               return Left.Value = Right.Value;
            when T_Iterator =>
               return False;
            when T_External =>
               return Left.External_Name = Right.External_Name
                 and then Left.External_Imm = Right.External_Imm;
            when T_Text =>
               return Left.Text = Right.Text;
            when T_Shelf =>
               return Left.Shelf_Name = Right.Shelf_Name;
         end case;
      end if;
   end Same_Operand;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (T       : in out Transfer;
                        Label   : in     Tagatha.Labels.Tagatha_Label)
   is
   begin
      if Tagatha.Labels.Has_Label (T.Label) then
         Tagatha.Labels.Link_To (T.Label, Label);
      else
         T.Label := Label;
      end if;
   end Set_Label;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Item   : in out Transfer;
      Line   : Positive;
      Column : Positive)
   is
   begin
      Item.Line := Line;
      Item.Column := Column;
   end Set_Location;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Item : in out Transfer_Operand;
                       Size : in     Tagatha_Size)
   is
   begin
      Item.Modifiers.Have_Size := True;
      Item.Modifiers.Size     := Size;
   end Set_Size;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Item : in out Transfer;
                       Size : in Tagatha_Size)
   is
   begin
      Set_Size (Item.Dst, Size);
   end Set_Size;

   ----------
   -- Show --
   ----------

   function Show (Item : Transfer) return String is
      use Ada.Strings, Ada.Strings.Fixed;
      Label_Image : constant String :=
                      Tagatha.Labels.Show_All
                        (Item.Label, 'L')
                      & "["
                      & Trim (Item.Line'Img, Left)
                      & ","
                      & Trim (Item.Column'Img, Left)
                      & "]";
   begin
      case Item.Trans is
         when T_Change_Stack =>
            if Item.Reserve < 0 then
               return Label_Image
                 & "close_frame" & Integer'Image (-Item.Reserve);
            else
               return Label_Image
                 & "open_frame" & Integer'Image (Item.Reserve);
            end if;
         when T_Control =>
            return Label_Image
              & "jump " & Tagatha_Condition'Image (Item.Condition)
              & " "
              & (if Tagatha.Labels.Has_Label (Item.Destination)
                 then Tagatha.Labels.Show (Item.Destination, 'L')
                 else "<>");
         when T_Data =>
            declare
               Dst    : constant String := Show (Item.Dst);
               Src_1  : constant String := Show (Item.Src_1);
               Src_2  : constant String := Show (Item.Src_2);
               Op     : constant String := Show_Operator (Item.Op);
               Sz     : Character;
            begin
               case Item.Dst.Modifiers.Size is
                  when Size_8 =>
                     Sz := '1';
                  when Size_16 =>
                     Sz := '2';
                  when Size_32 =>
                     Sz := '4';
                  when Size_64 =>
                     Sz := '8';
                  when Default_Size =>
                     Sz := 'd';
                  when Default_Integer_Size =>
                     Sz := 'i';
                  when Default_Address_Size =>
                     Sz := 'a';
               end case;

               if Item.Op = Op_Nop then
                  return Label_Image & Sz & ':' & Dst & " := " & Src_1;
               else
                  return Label_Image & Sz & ':' & Dst & " := " &
                  Src_1 & " " & Op & " " & Src_2;
               end if;
            end;
         when T_Native =>
            return Label_Image
              & Ada.Strings.Unbounded.To_String (Item.Native);
      end case;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Item : Transfer_Operand) return String is
      Indirect : constant String :=
                   (if Item.Modifiers.Indirect then "&" else "");

      function Image return String;

      -----------
      -- Image --
      -----------

      function Image return String is
      begin
         case Item.Op is
         when T_No_Operand =>
            return "<>";
         when T_Stack =>
            return "<stack>";
         when T_Condition =>
            return "<cc>";
         when T_Temporary =>
            return Temporaries.Show (Item.Temp);
         when T_Argument =>
            return "arg" &
              Integer'Image (-1 * Integer (Item.Arg_Offset));
         when T_Result =>
            return "result";
         when T_Return =>
            return "return";
         when T_Iterator =>
            if Item.New_Iterator then
               return "new iterator";
            elsif Item.Copy_Iterator then
               return "(iterator)+";
            else
               return "iterator";
            end if;
         when T_Local =>
            return "frm" &
              Integer'Image (-1 * Integer (Item.Loc_Offset));
         when T_Immediate =>
            return Tagatha.Constants.Show (Item.Value);
         when T_External =>
            return (if Item.External_Imm then "#" else "")
              & Ada.Strings.Unbounded.To_String (Item.External_Name);
         when T_Text =>
            return """"
              & Ada.Strings.Unbounded.To_String (Item.Text)
              & """";
         when T_Shelf =>
            return "shelf["
              & Ada.Strings.Unbounded.To_String (Item.Shelf_Name)
              & "]";
         end case;
      end Image;

   begin
      return Indirect & Image;
   end Show;

   -------------------
   -- Show_Operator --
   -------------------

   function Show_Operator (Item : Tagatha_Operator) return String is
   begin
      return Tagatha_Operator'Image (Item);
   end Show_Operator;

   ---------------------
   -- Simple_Transfer --
   ---------------------

   function Simple_Transfer (From          : Transfer_Operand;
                             To            : Transfer_Operand;
                             To_Address    : Boolean := False)
                             return Transfer
   is
   begin
      return (Trans             => T_Data,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => C_Always,
              Destination       => Tagatha.Labels.No_Label,
              Native            => Ada.Strings.Unbounded.Null_Unbounded_String,
              Changed_Registers => Ada.Strings.Unbounded.Null_Unbounded_String,
              Line              => 0,
              Column            => 0,
              Self              => Same_Operand (From, To),
              Argument_Count    => 0,
              Call              => False,
              Src_1             => From,
              Src_2             => No_Operand,
              Dst               => To,
              To_Address        => To_Address,
              Op                => Op_Nop);
   end Simple_Transfer;

   -----------
   -- Slice --
   -----------

   function Slice (Item   : Transfer_Operand;
                   Offset : Natural;
                   Size   : Tagatha_Size)
                  return Transfer_Operand
   is
      Result : Transfer_Operand := Item;
   begin
      Result.Modifiers.Have_Slice := True;
      Result.Modifiers.Slice      :=
        (Tagatha_Integer (Offset * Size_Bits (Size)),
         Tagatha_Integer ((Offset + 1) * Size_Bits (Size) - 1));
      return Result;
   end Slice;

   ----------------
   -- Slice_Fits --
   ----------------

   function Slice_Fits (Item : Transfer_Operand;
                        Size : Tagatha_Size)
                       return Boolean
   is
   begin
      return Get_Slice_Bit_Length (Item) <= 2**(Tagatha_Size'Pos (Size) + 3);
   end Slice_Fits;

   -------------------
   -- Stack_Operand --
   -------------------

   function Stack_Operand return Transfer_Operand is
   begin
      return (T_Stack, No_Modification);
   end Stack_Operand;

   -----------------------
   -- Temporary_Operand --
   -----------------------

   function Temporary_Operand
     (Temp       : Tagatha.Temporaries.Temporary;
      Indirect   : Boolean := False)
      return Transfer_Operand
   is
   begin
      return (T_Temporary,
              (No_Modification with delta Dereferenced => Indirect),
              Temp);
   end Temporary_Operand;

   ------------------
   -- Text_Operand --
   ------------------

   function Text_Operand
     (Text      : String)
      return Transfer_Operand
   is
   begin
      return (T_Text, No_Modification,
              Ada.Strings.Unbounded.To_Unbounded_String (Text));
   end Text_Operand;

   ------------------
   -- To_Temporary --
   ------------------

   function To_Temporary (Src_1, Src_2 : Transfer_Operand;
                          Op           : Tagatha_Operator;
                          Dst          : Temporaries.Temporary)
                          return Transfer
   is
   begin
      return (Trans             => T_Data,
              Reserve           => 0,
              Label             => Tagatha.Labels.No_Label,
              Condition         => C_Always,
              Destination       => Tagatha.Labels.No_Label,
              Native            => Ada.Strings.Unbounded.Null_Unbounded_String,
              Changed_Registers => Ada.Strings.Unbounded.Null_Unbounded_String,
              Line              => 0,
              Column            => 0,
              Argument_Count    => 0,
              Self              => False,
              Call              => False,
              Src_1             => Src_1,
              Src_2             => Src_2,
              Dst               => (T_Temporary, No_Modification, Dst),
              To_Address        => False,
              Op                => Op);
   end To_Temporary;

end Tagatha.Transfers;
