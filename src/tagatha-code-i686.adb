with Ada.Strings.Fixed;
--  with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Constants;
with Tagatha.Labels;

package body Tagatha.Code.I686 is

   function Get_Mnemonic (Op : Tagatha_Operator) return String;

   function Image (Item : Tagatha_Integer) return String;

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String;
                          Source   : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String);

   procedure Move (Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move (Asm           : in out Assembly'Class;
                   Transfer_Size : in     Tagatha_Size;
                   Source        : in     String;
                   Dest          : in     String);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm    : in out Assembly'Class;
      Op     : in     Tagatha_Operator;
      Source : in     Tagatha.Transfers.Transfer_Operand;
      Dest   : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     String;
      Source_2 : in     String;
      Dest     : in     String);

   function To_Dereferenced_String
     (Item        : Tagatha.Transfers.Transfer_Operand)
      return String;

   function To_String (Item        : Tagatha.Transfers.Transfer_Operand)
                       return String;

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                       return String;

   function To_Integer is new
     Ada.Unchecked_Conversion (Tagatha_Floating_Point,
                               Floating_Point_Integer);

   ------------
   -- Encode --
   ------------

   overriding
   procedure Encode
     (T    : in out I686_Translator;
      Asm  : in out Assembly'Class;
      Item : in     Tagatha.Transfers.Transfer)
   is
      use Tagatha.Transfers;
      use type Tagatha.Labels.Tagatha_Label;
      Label : Tagatha.Labels.Tagatha_Label := Get_Label (Item);
   begin

      --  Asm.Put_Line ("        ; " & Tagatha.Transfers.Show (Item));

      while Label /= Tagatha.Labels.No_Label loop
         Asm.Put_Line (Tagatha.Labels.Show (Label, '_') & ":");
         Label := Tagatha.Labels.Next_Linked_Label (Label);
      end loop;

      if Is_Control (Item) then
         declare
            use Tagatha.Labels;
            Cond : constant Tagatha_Condition := Get_Condition (Item);
            Dest : constant Tagatha_Label     := Get_Destination (Item);
         begin
            Asm.Put_Line ("    j" & To_String (Cond, T.Reverse_Test) &
                            " " & Show (Dest, '_'));
            T.Reverse_Test := False;
         end;
      elsif Is_Frame_Reservation (Item) then
         declare
            Reservation : constant String :=
                            Integer'Image (Get_Reservation (Item) * 4);
         begin
            if Get_Reservation (Item) > 0 then
               Asm.Put_Line ("    subl $" &
                             Reservation (2 .. Reservation'Last) &
                             ", %esp");
            end if;
         end;
      elsif Is_Simple (Item) then
         Move (Asm, Get_Source (Item), Get_Destination (Item));
      elsif Get_Operator (Item) in One_Argument_Operator then
         if Same_Operand (Get_Source (Item), Get_Destination (Item)) then
            Operate (Asm, Get_Operator (Item),
                     Get_Destination (Item));
         else
            Operate (Asm, Get_Operator (Item),
                     Get_Source (Item), Get_Destination (Item));
         end if;
      else
         if Get_Operator (Item) = Op_Compare then
            Operate (Asm, Op_Compare,
                     Get_Source_2 (Item), Get_Source_1 (Item));
         else
            Operate (Asm, Get_Operator (Item),
                     To_String (Get_Source_2 (Item)),
                     To_String (Get_Source_1 (Item)),
                     To_String (Get_Destination (Item)));
         end if;

      end if;
   end Encode;

   ------------
   -- Finish --
   ------------

   overriding
   procedure Finish
     (T   : in out I686_Translator;
      Asm : in out Assembly'Class)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line ("    leave");
      Asm.Put_Line ("    ret");
   end Finish;

   ------------------
   -- Get_Mnemonic --
   ------------------

   function Get_Mnemonic (Op : Tagatha_Operator) return String is
   begin
      case Op is
         when Op_Nop =>
            return "movl";
         when Op_Add =>
            return "add";
         when Op_Sub =>
            return "sub";
         when Op_Mul =>
            return "imul";
         when Op_Div =>
            return "idiv";
         when Op_Mod =>
            return "imod";
         when Op_And =>
            return "and";
         when Op_Or =>
            return "or";
         when Op_Xor =>
            return "xor";
         when Op_Not =>
            return "not";
         when Op_Bit_Test =>
            return "and";
         when Op_Bit_Clear =>
            return "bic";
         when Op_Bit_Set =>
            return "bis";
         when Op_Equal .. Op_Less_Equal =>
            raise Constraint_Error with
              "compare operators not implemented on i686";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the i686";
         when Op_Compare =>
            return "cmp";
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the i586";
         when Op_Negate =>
            return "neg";
         when Op_Complement =>
            return "not";
         when Op_Test =>
            return "cmp";
         when Op_Logical_Shift =>
            raise Constraint_Error with
              "we didn't implement logical shifts yet";
         when Op_Dereference =>
            raise Constraint_Error with
              "should not be getting a mnemonic for dereference";
      end case;
   end Get_Mnemonic;

   ------------------------
   -- Get_Register_Range --
   ------------------------

   overriding function Get_Register_Range
     (Translator : I686_Translator;
      Data       : Tagatha_Data_Type)
      return Register_Range_Record
   is
   begin
      case Data is
         when Untyped_Data | Address_Data =>
            return (1, 4);
         when Floating_Point_Data =>
            return (5, 8);
      end case;
   end Get_Register_Range;

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator return Translator'Class is
      Result : I686_Translator;
   begin
      return Translator'Class (Result);
   end Get_Translator;

   -----------
   -- Image --
   -----------

   function Image (Item : Tagatha_Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Tagatha_Integer'Image (Item),
                                     Ada.Strings.Left);
   end Image;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String;
                          Source   : in     String)
   is
   begin
      Asm.Put_Line ("    " & Mnemonic & " " & Dest & ", " & Source);
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String)
   is
   begin
      Asm.Put_Line ("    " & Mnemonic & " " & Dest);
   end Instruction;

   -----------
   -- Label --
   -----------

   overriding
   procedure Label (T     : in out I686_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (T);
      use type Tagatha.Labels.Tagatha_Label;
      L : Tagatha.Labels.Tagatha_Label := Label;

   begin

      while L /= Tagatha.Labels.No_Label loop
         Asm.Put_Line (Tagatha.Labels.Show (L, '_') & ":");
         L := Tagatha.Labels.Next_Linked_Label (L);
      end loop;

   end Label;

   ----------
   -- Move --
   ----------

   procedure Move (Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Transfer_Size : constant Tagatha_Size := Get_Size (Dest);
   begin
      Move (Asm, Transfer_Size, To_String (Source), To_String (Dest));
   end Move;

   ----------
   -- Move --
   ----------

   procedure Move (Asm           : in out Assembly'Class;
                   Transfer_Size : Tagatha_Size;
                   Source        : in     String;
                   Dest          : in     String)
   is
   begin
      if Transfer_Size = Size_8 then
         Instruction (Asm, "movb", Source, Dest);
      elsif Transfer_Size = Size_16 then
         Instruction (Asm, "movw", Source, Dest);
      elsif Transfer_Size in
        Size_32 | Default_Size | Default_Integer_Size | Default_Address_Size
      then
         if Source (Source'First) = '%'
           or else Dest (Dest'First) = '%'
         then
            Instruction (Asm, "movl", Source, Dest);
         else
            Instruction (Asm, "movl", Source, "%eax");
            Instruction (Asm, "movl", "%eax", Dest);
         end if;
      else
         raise Constraint_Error with
           "sizes > 32 bits not handled yet";
      end if;
   end Move;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      case Op is
         when Op_Negate =>
            Instruction (Asm, "neg", To_String (Dest));
         when Op_Not =>
            Instruction (Asm, "cmpl", "$0", To_String (Dest));
         when Op_Complement =>
            Instruction (Asm, "not", To_String (Dest));
         when Op_Test =>
            Instruction (Asm, "cmpl", "$0", To_String (Dest));
         when Op_Dereference =>
            Instruction (Asm, "movl", To_Dereferenced_String (Dest),
                         To_String (Dest));
      end case;

   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      if Op = Op_Dereference then
         Instruction (Asm, "movl", To_String (Dest),
                      To_Dereferenced_String (Source));
      else
         Operate (Asm, Op, To_String (Dest), To_String (Source),
                  To_String (Dest));
      end if;
   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     String;
      Source_2 : in     String;
      Dest     : in     String)
   is
   begin
      case Op is
         when Op_Div | Op_Mod =>
            Instruction (Asm, "movl", Source_1, "%edx");
            Instruction (Asm, "movl", "%edx", "%eax");
            Instruction (Asm, "sarl", "$31", "%edx");
            Instruction (Asm, "idivl", Source_2);
            if Op = Op_Div then
               Instruction (Asm, "movl", "%edx", Dest);
            else
               Instruction (Asm, "movl", "%eax", Dest);
            end if;
         when others =>
            if Source_1 = Dest then
               Instruction (Asm, Get_Mnemonic (Op), Source_2, Dest);
            else
               null;
            end if;
      end case;
   end Operate;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start
     (T      : in out I686_Translator;
      Asm    : in out Assembly'Class;
      Name   : in     String;
      Global : in     Boolean)
   is
      pragma Unreferenced (T);
   begin
      if Global then
         Asm.Put_Line (".globl " & Name);
      end if;

      Asm.Put_Line ('_' & Name & ":");
      Asm.Put_Line ("    pushl %ebp");
      Asm.Put_Line ("    movl %esp, %ebp");
   end Start;

   ----------------------------
   -- To_Dereferenced_String --
   ----------------------------

   function To_Dereferenced_String
     (Item        : Tagatha.Transfers.Transfer_Operand)
     return String
   is
   begin
      return "@" & To_String (Item);
   end To_Dereferenced_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Tagatha.Transfers.Transfer_Operand)
        return String
   is
      use Tagatha.Transfers;
   begin
      if Is_Constant (Item) then
         declare
            use Tagatha.Constants;
            V : constant Tagatha_Constant := Get_Value (Item);
         begin
            if Is_Integer (V) then
               return '$' &
               Image ((Get_Integer (V) and Get_Slice_Mask (Item)) /
                      (2 ** Natural (Get_Slice_Bit_Offset (Item))));
            elsif Is_Floating_Point (V) then
               return '$' &
               Floating_Point_Integer'Image
                 (To_Integer (Get_Floating_Point (V)));
            elsif Is_Label (V) then
               if Has_Slice (Item) then
                  if Slice_Fits (Item, Size_8) then
                     return Tagatha.Labels.Show (Get_Label (V), '_') & " +" &
                       Image (Get_Slice_Octet_Offset (Item));
                  else
                     raise Constraint_Error with
                       "can't take non-Octet slice from label: " &
                       Show (Item);
                  end if;
               else
                  return Tagatha.Labels.Show (Get_Label (V), '_');
               end if;
            else
               raise Constraint_Error with
                 "unknown constant type in " & Show (V);
            end if;
         end;
      elsif Is_Argument (Item) or else Is_Local (Item) then
         declare
            Addr : Tagatha_Integer;
         begin
            if Is_Argument (Item) then
               Addr := Tagatha_Integer (Get_Arg_Offset (Item) * 4 + 4);
            else
               Addr := Tagatha_Integer (Get_Local_Offset (Item) * 4);
            end if;
            if Has_Slice (Item) then
               if Slice_Fits (Item, Size_8) then
                  return Image (Addr + Get_Slice_Octet_Offset (Item)) &
                  "(%ebp)";
               elsif Is_Argument (Item) then
                  return Image (Addr) & "(%ebp)";
               else
                  return "-" & Image (Addr) & "(%ebp)";
               end if;
            elsif Is_Argument (Item) then
               return Image (Addr) & "(%ebp)";
            else
               return "-" & Image (Addr) & "(%ebp)";
            end if;
         end;
      elsif Is_Result (Item) then
         return "%eax";
      else
         raise Constraint_Error with
           "unknown operand type in " & Show (Item);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                      return String
   is
      function Actual_Condition return Tagatha_Condition;

      ----------------------
      -- Actual_Condition --
      ----------------------

      function Actual_Condition return Tagatha_Condition is
      begin
         if Negated then
            case Cond is
               when C_Always =>
                  raise Constraint_Error with
                    "Cannot negate ""always"" condition";
               when C_Equal =>
                  return C_Not_Equal;
               when C_Not_Equal =>
                  return C_Equal;
               when C_Greater =>
                  return C_At_Most;
               when C_Less =>
                  return C_At_Least;
               when C_At_Most =>
                  return C_Greater;
               when C_At_Least =>
                  return C_Less;
            end case;
         else
            return Cond;
         end if;
      end Actual_Condition;

   begin
      case Actual_Condition is
         when C_Always =>
            return "mp";
         when C_Equal =>
            return "e";
         when C_Not_Equal =>
            return "ne";
         when C_Greater =>
            return "gt";
         when C_Less =>
            return "lt";
         when C_At_Least =>
            return "ge";
         when C_At_Most =>
            return "le";
      end case;
   end To_String;

   ---------------
   -- Word_Size --
   ---------------

   overriding
   function Word_Size
     (T : I686_Translator)
      return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_32;
   end Word_Size;

end Tagatha.Code.I686;
