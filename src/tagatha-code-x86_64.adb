with Ada.Strings.Fixed;
--  with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Constants;
with Tagatha.Labels;

package body Tagatha.Code.X86_64 is

   Preallocated_Frame : constant := 112;

   function Get_Mnemonic (Op : Tagatha_Operator) return String;

   function Image (Item : Tagatha_Integer) return String;

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String;
                          Source   : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String);

   procedure Move (T         : in out X86_64_Translator;
                   Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move (T             : in out X86_64_Translator;
                   Asm           : in out Assembly'Class;
                   Transfer_Size : in     Tagatha_Size;
                   Source        : in     String;
                   Dest          : in     String);

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op     : in     Tagatha_Operator;
      Source : in     Tagatha.Transfers.Transfer_Operand;
      Dest   : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     String;
      Source_2 : in     String;
      Dest     : in     String);

   function To_Dereferenced_String
     (T        : X86_64_Translator;
      Item     : Tagatha.Transfers.Transfer_Operand)
      return String;

   function To_String (T    : X86_64_Translator;
                       Item : Tagatha.Transfers.Transfer_Operand)
        return String;

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                       return String;

   function To_String (Value : Integer) return String;

   function To_Integer is new
     Ada.Unchecked_Conversion (Tagatha_Floating_Point,
                               Floating_Point_Integer);

   -----------------
   -- Begin_Frame --
   -----------------

   overriding
   procedure Begin_Frame (T           : in out X86_64_Translator;
                          Asm         : in out Assembly'Class;
                          Return_Count    : Natural;
                          Arg_Count       : Natural;
                          Local_Count     : Natural;
                          Temporary_Count : Natural)
   is
      pragma Unreferenced (Return_Count, Temporary_Count);
      Arg_Reg : constant array (1 .. 6) of String (1 .. 3) :=
        ("edi", "esi", "edx", "ecx", "r8d", "r9d");
   begin
      T.Arg_Size := Arg_Count * 4;
      T.Frame_Size := Local_Count * 4;
      if T.Arg_Size + T.Frame_Size > Preallocated_Frame then
         declare
            Extra_Frame : Natural := T.Arg_Size + T.Frame_Size -
              Preallocated_Frame;
         begin
            if Extra_Frame mod 8 /= 0 then
               Extra_Frame := Extra_Frame + 8 - Extra_Frame mod 8;
            end if;
            Asm.Put_Line ("    subq $" & To_String (Extra_Frame) &
                            ", %rsp");
         end;
      end if;

      for I in Arg_Reg'Range loop
         exit when I > Arg_Count;
         Asm.Put_Line ("    movl %" & Arg_Reg (I) & ", -" &
                         To_String (I * 4) & "(%rbp)");
      end loop;

   end Begin_Frame;

   ------------
   -- Encode --
   ------------

   overriding
   procedure Encode
     (T    : in out X86_64_Translator;
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
         null;
      elsif Is_Simple (Item) then
         Move (T, Asm, Get_Source (Item), Get_Destination (Item));
      elsif Get_Operator (Item) in One_Argument_Operator then
         if Same_Operand (Get_Source (Item), Get_Destination (Item)) then
            Operate (T, Asm, Get_Operator (Item),
                     Get_Destination (Item));
         else
            Operate (T, Asm, Get_Operator (Item),
                     Get_Source (Item), Get_Destination (Item));
         end if;
      else
         if Get_Operator (Item) = Op_Compare then
            Operate (T, Asm, Op_Compare,
                     Get_Source_2 (Item), Get_Source_1 (Item));
         else
            Operate (T, Asm, Get_Operator (Item),
                     To_String (T, Get_Source_2 (Item)),
                     To_String (T, Get_Source_1 (Item)),
                     To_String (T, Get_Destination (Item)));
         end if;

      end if;
   end Encode;

   -------------------
   -- File_Preamble --
   -------------------

   overriding
   procedure File_Preamble (T                : in out X86_64_Translator;
                            Asm              : in out Assembly'Class;
                            Source_File_Name : in     String)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line ("    .file   """ &
                      Source_File_Name &
                      """");
      Asm.Put_Line ("    .text");
   end File_Preamble;

   ------------
   -- Finish --
   ------------

   overriding
   procedure Finish
     (T   : in out X86_64_Translator;
      Asm : in out Assembly'Class)
   is
      Name : constant String :=
        Ada.Strings.Unbounded.To_String (T.Current_Unit_Name);
   begin
      Asm.Put_Line ("    leave");
      Asm.Put_Line ("    ret");
      Asm.Put_Line ("    .cfi_endproc");
      Asm.Put_Line (".LFE0:");
      Asm.Put_Line ("    .size " & Name & ", .-" & Name);
      Asm.Put_Line ("    .ident ""Tagatha""");
   end Finish;

   -----------------------
   -- General_Registers --
   -----------------------

   overriding
   function General_Registers
     (T : X86_64_Translator)
      return Positive
   is
      pragma Unreferenced (T);
   begin
      return 4;
   end General_Registers;

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
              "compare operators not implemented on x86_64";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the x86_64";
         when Op_Compare =>
            return "cmpl";
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

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator return Translator'Class is
      Result : constant X86_64_Translator :=
        (Current_Unit_Name => Ada.Strings.Unbounded.Null_Unbounded_String,
         Reverse_Test      => False,
         Arg_Size          => 0,
         Frame_Size        => 0);
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

   overriding procedure Label (T     : in out X86_64_Translator;
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

   procedure Move (T         : in out X86_64_Translator;
                   Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Transfer_Size : Tagatha_Size := Size_32;
   begin
      if Has_Size (Dest) then
         Transfer_Size := Get_Size (Dest);
      end if;
      Move (T, Asm, Transfer_Size,
            To_String (T, Source), To_String (T, Dest));
   end Move;

   ----------
   -- Move --
   ----------

   procedure Move (T             : in out X86_64_Translator;
                   Asm           : in out Assembly'Class;
                   Transfer_Size : Tagatha_Size;
                   Source        : in     String;
                   Dest          : in     String)
   is
      pragma Unreferenced (T);
   begin
      case Transfer_Size is
         when Size_8 =>
            Instruction (Asm, "movb", Source, Dest);
         when Size_16 =>
            Instruction (Asm, "movw", Source, Dest);
         when Size_32
            | Default_Size | Default_Integer_Size | Default_Address_Size =>
            if Source (Source'First) = '%' or else
              Dest (Dest'First) = '%'
            then
               Instruction (Asm, "movl", Source, Dest);
            else
               Instruction (Asm, "movl", Source, "%eax");
               Instruction (Asm, "movl", "%eax", Dest);
            end if;
         when Size_64 =>
            raise Constraint_Error with
              "64 bit arguments not handled yet";
      end case;
   end Move;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      case Op is
         when Op_Negate =>
            Instruction (Asm, "neg", To_String (T, Dest));
         when Op_Not =>
            Instruction (Asm, "not", To_String (T, Dest));
         when Op_Complement =>
            Instruction (Asm, "not", To_String (T, Dest));
         when Op_Test =>
            Instruction (Asm, "cmpl", "$0", To_String (T, Dest));
         when Op_Dereference =>
            Instruction (Asm, "movl", To_Dereferenced_String (T, Dest),
                         To_String (T, Dest));
      end case;

   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      if Op = Op_Dereference then
         Instruction (Asm, "movl", To_String (T, Dest),
                      To_Dereferenced_String (T, Source));
      else
         Operate (T, Asm, Op, To_String (T, Dest), To_String (T, Source),
                  To_String (T, Dest));
      end if;
   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (T        : in out X86_64_Translator;
      Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     String;
      Source_2 : in     String;
      Dest     : in     String)
   is
      pragma Unreferenced (T);
   begin
      case Op is
         when Op_Div | Op_Mod =>
            Instruction (Asm, "movl", Source_2, "%eax");
            Instruction (Asm, "movl", "%eax", "%edx");
            Instruction (Asm, "sarl", "$31", "%edx");
            Instruction (Asm, "idivl", Source_1);
            if Op = Op_Div then
               Instruction (Asm, "movl", "%eax", Dest);
            else
               Instruction (Asm, "movl", "%edx", Dest);
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
     (T      : in out X86_64_Translator;
      Asm    : in out Assembly'Class;
      Name   : in     String;
      Global : in     Boolean)
   is
      pragma Unreferenced (T);
   begin
      T.Current_Unit_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      if Global then
         Asm.Put_Line (".globl " & Name);
      end if;
      Asm.Put_Line ("    .type   " & Name & ", @function");
      Asm.Put_Line (Name & ":");
      Asm.Put_Line (".LFB0:");
      Asm.Put_Line ("    .cfi_startproc");
      Asm.Put_Line ("    pushq   %rbp");
      Asm.Put_Line ("    .cfi_def_cfa_offset 16");
      Asm.Put_Line ("    movq   %rsp, %rbp");
      Asm.Put_Line ("    .cfi_offset 6, -16");
      Asm.Put_Line ("    .cfi_def_cfa_register 6");
   end Start;

   ----------------------------
   -- To_Dereferenced_String --
   ----------------------------

   function To_Dereferenced_String
     (T        : X86_64_Translator;
      Item     : Tagatha.Transfers.Transfer_Operand)
     return String
   is
   begin
      return "@" & To_String (T, Item);
   end To_Dereferenced_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (T    : X86_64_Translator;
                       Item : Tagatha.Transfers.Transfer_Operand)
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
               declare
                  Slice_Mask       : constant Tagatha_Integer :=
                    Get_Slice_Mask (Item);
                  Slice_Bit_Offset : constant Tagatha_Integer :=
                    Get_Slice_Bit_Offset (Item);
                  Value            : constant Tagatha_Integer :=
                    Get_Integer (V);
               begin
                  return '$' & Image ((Value and Slice_Mask) /
                                        (2**Natural (Slice_Bit_Offset)));
               end;
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
               Addr := Tagatha_Integer (Get_Arg_Offset (Item) * 4);
            else
               Addr :=
                 Tagatha_Integer (Natural (Get_Local_Offset (Item)) * 4 +
                                    T.Arg_Size);
            end if;
            if Has_Slice (Item) then
               if Slice_Fits (Item, Size_8) then
                  return Image (Addr + Get_Slice_Octet_Offset (Item)) &
                  "(%rbp)";
               else
                  return "-" & Image (Addr) & "(%rbp)";
               end if;
            else
               return "-" & Image (Addr) & "(%rbp)";
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
   -- To_String --
   ---------------

   function To_String (Value : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (Value),
                                     Ada.Strings.Both);
   end To_String;

   ---------------
   -- Word_Size --
   ---------------

   overriding
   function Word_Size
     (T : X86_64_Translator)
      return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_32;
   end Word_Size;

end Tagatha.Code.X86_64;
