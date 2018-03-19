with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Temporaries;

package body Tagatha.Code.Pdp32 is

   Result_Register : constant String := "r0";

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                      return String;

   procedure Move (Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Dest     : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Source   : in     String;
                          Dest     : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Source_1 : in     String;
                          Source_2 : in     String;
                          Dest     : in     String);

   function Get_Mnemonic (Op : Tagatha_Operator) return String;
   function Get_Reversed (Op : Tagatha_Operator) return Boolean;

   function To_String
     (Item : Tagatha.Transfers.Transfer_Operand;
      Source : Boolean)
      return String;

   function To_Src (Item : Tagatha.Transfers.Transfer_Operand) return String
   is (To_String (Item, True));

   function To_Dst (Item : Tagatha.Transfers.Transfer_Operand) return String
   is (To_String (Item, False));

   function To_Dereferenced_String (Item : Tagatha.Transfers.Transfer_Operand)
                                   return String;

   function To_Suffix
     (Size : Tagatha_Size)
      return String;

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     Tagatha.Transfers.Transfer_Operand;
      Source_2 : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   ------------
   -- Encode --
   ------------

   overriding procedure Encode
     (T    : in out Pdp32_Translator;
      Asm  : in out Assembly'Class;
      Item : in     Tagatha.Transfers.Transfer)
   is
      use Tagatha.Transfers;
      use type Tagatha.Labels.Tagatha_Label;
      Label : Tagatha.Labels.Tagatha_Label := Get_Label (Item);
   begin

      while Label /= Tagatha.Labels.No_Label loop
         Asm.Put_Line (Tagatha.Labels.Show (Label, 'L') & ":");
         Label := Tagatha.Labels.Next_Linked_Label (Label);
      end loop;

      if Is_Control (Item) then
         declare
            use Tagatha.Labels;
            Cond : constant Tagatha_Condition := Get_Condition (Item);
            Dest : constant Tagatha_Label     := Get_Destination (Item);
         begin
            Asm.Put_Line ("    b" & To_String (Cond, T.Reverse_Test) &
                            " " & Show (Dest, 'L'));
            T.Reverse_Test := False;
         end;
      elsif Is_Call (Item) then
         declare
            use Tagatha.Labels;
            Dest : constant Tagatha_Label := Get_Destination (Item);
         begin
            if Dest = No_Label then
               Asm.Put_Line
                 ("    call 0,(sp)+");
            else
               Asm.Put_Line
                 ("    jsr " & Tagatha.Labels.Show (Dest, 'L'));
            end if;
         end;
      elsif Is_Frame_Reservation (Item) then
         declare
            Reservation : constant String :=
              Integer'Image (Get_Reservation (Item) * 4);
            Operation   : String := "add";
         begin
            if Get_Reservation (Item) /= 0 then
               if Get_Reservation (Item) > 0 then
                  Operation := "sub";
               end if;
               Asm.Put_Line ("    " & Operation & " #" &
                               Reservation (2 .. Reservation'Last) &
                               ", sp");
            end if;
         end;

      elsif Is_Native (Item) then
         Asm.Put_Line ("    " & Get_Native_Text (Item));
      elsif Is_Simple (Item) then
         Move (Asm, Get_Source (Item), Get_Destination (Item));
      elsif Get_Operator (Item) = Op_Test then
         Instruction (Asm, "tst",
                      Get_Size (Get_Source_1 (Item)),
                      To_String (Get_Source_1 (Item), True));
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
            if Is_Constant_Zero (Get_Source_2 (Item)) then
               Instruction (Asm, "tst",
                            Get_Size (Get_Source_1 (Item)),
                            To_String (Get_Source_1 (Item), True));
            else
               Operate (Asm, Op_Compare,
                        Get_Source_1 (Item), Get_Source_2 (Item));
            end if;
            --  T.Reverse_Test := True;
         elsif Same_Operand (Get_Source_2 (Item), Get_Destination (Item))
           and then not Is_Stack (Get_Destination (Item))
         then
            Operate (Asm, Get_Operator (Item), Get_Source_1 (Item),
                     Get_Destination (Item));
         else
            Operate (Asm, Get_Operator (Item),
                     Get_Source_1 (Item),
                     Get_Source_2 (Item),
                     Get_Destination (Item));
         end if;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "failed to encode instruction: " & Transfers.Show (Item));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));

   end Encode;

   -------------------
   -- File_Preamble --
   -------------------

   overriding procedure File_Preamble
     (T                : in out Pdp32_Translator;
      Asm              : in out Assembly'Class;
      Source_File_Name : in     String)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line (".source_file """ & Source_File_Name & """");
   end File_Preamble;

   ------------
   -- Finish --
   ------------

   overriding
   procedure Finish (T   : in out Pdp32_Translator;
                     Asm : in out Assembly'Class)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line ("    mov fp, sp");
      Asm.Put_Line ("    mov (sp)+, fp");
      Asm.Put_Line ("    rts");
   end Finish;

   -----------------------
   -- General_Registers --
   -----------------------

   overriding
   function General_Registers (T : Pdp32_Translator) return Positive is
      pragma Unreferenced (T);
   begin
      --  Registers are used for argument passing, so we
      --  only reserve a small number of general purpose ones.
      --  R13 is the frame pointer.
      --  R14 is the stack, R15 is the PC
      return 2;
   end General_Registers;

   ------------------
   -- Get_Mnemonic --
   ------------------

   function Get_Mnemonic (Op : Tagatha_Operator) return String is
   begin
      case Op is
         when Op_Nop =>
            return "mov";
         when Op_Add =>
            return "add";
         when Op_Sub =>
            return "sub";
         when Op_Mul =>
            return "mul";
         when Op_Div =>
            return "div";
         when Op_Mod =>
            return "mod";
         when Op_And =>
            return "and";
         when Op_Or =>
            return "or";
         when Op_Xor =>
            return "xor";
         when Op_Not =>
            return "not";
         when Op_Bit_Test =>
            return "bit";
         when Op_Bit_Clear =>
            return "bic";
         when Op_Bit_Set =>
            return "bis";
         when Op_Equal =>
            return "seq";
         when Op_Not_Equal =>
            return "sne";
         when Op_Greater =>
            return "sgt";
         when Op_Less =>
            return "slt";
         when Op_Greater_Equal =>
            return "sge";
         when Op_Less_Equal =>
            return "sle";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the pdp-32";
         when Op_Compare =>
            return "cmp";
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the pdp-32";
         when Op_Negate =>
            return "neg";
         when Op_Complement =>
            return "not";
         when Op_Test =>
            return "tst";
         when Op_Dereference =>
            raise Constraint_Error with
              "should not be getting a mnemonic for dereference";
      end case;
   end Get_Mnemonic;

   ------------------
   -- Get_Reversed --
   ------------------

   function Get_Reversed (Op : Tagatha_Operator) return Boolean is
   begin
      return Op = Op_Sub;
   end Get_Reversed;

   -------------------
   -- Get_Translator --
   --------------------

   function Get_Translator return Translator'Class is
      Result : Pdp32_Translator;
   begin
      return Translator'Class (Result);
   end Get_Translator;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Source   : in     String;
                          Dest     : in     String)
   is
   begin
      Asm.Put_Line
        ("    " & Mnemonic & To_Suffix (Size)
         & " " & Source & ", " & Dest);
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Source_1 : in     String;
                          Source_2 : in     String;
                          Dest     : in     String)
   is
   begin
      Asm.Put_Line
        ("    " & Mnemonic & To_Suffix (Size)
         & " " & Source_1 & ", " & Source_2 & ", " & Dest);
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Size     : in     Tagatha_Size;
                          Dest     : in     String)
   is
   begin
      Asm.Put_Line ("    " & Mnemonic & To_Suffix (Size) & " " & Dest);
   end Instruction;

   -----------
   -- Label --
   -----------

   overriding
   procedure Label (T     : in out Pdp32_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (T);
      use type Tagatha.Labels.Tagatha_Label;
      L : Tagatha.Labels.Tagatha_Label := Label;

   begin

      while L /= Tagatha.Labels.No_Label loop
         Asm.Put_Line (Tagatha.Labels.Show (L, 'L') & ":");
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
      Transfer_Size : Tagatha_Size := Size_32;
   begin
      if Has_Size (Dest) then
         Transfer_Size := Get_Size (Dest);
      end if;

      if Is_Null_Operand (Dest) then
         if Source = Stack_Operand then
            case Transfer_Size is
               when Size_8 | Size_16 | Size_32
                  | Default_Size
                  | Default_Integer_Size | Default_Address_Size =>
                  Asm.Put_Line ("    tst (sp)+");
               when Size_64 =>
                  Asm.Put_Line ("    add #8, sp");
            end case;
         end if;
      elsif Is_Condition_Operand (Dest) then
         Instruction (Asm, "tst", Transfer_Size, To_Src (Source));
      else
         Instruction (Asm, "mov", Transfer_Size,
                      To_Src (Source),
                      To_Dst (Dest));
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
      use Tagatha.Transfers;
      Size : constant Tagatha_Size := Get_Size (Dest);
   begin
      case Op is
         when Op_Negate =>
            Instruction (Asm, "neg", Size, To_Dst (Dest));
         when Op_Complement =>
            Instruction (Asm, "not", Size, To_Dst (Dest));
         when Op_Not =>
            Instruction (Asm, "not", Size, To_Dst (Dest));
         when Op_Test =>
            Instruction (Asm, "tst", Size, To_Dst (Dest));
         when Op_Dereference =>
            Instruction (Asm, "mov", Size,
                         To_Dereferenced_String (Dest),
                         To_Dst (Dest));
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
      use Tagatha.Constants;
      use Tagatha.Transfers;
      use Ada.Strings, Ada.Strings.Fixed;
      type Quick_Operation is
         record
            Source_Value : Tagatha_Constant;
            Op           : Tagatha_Operator;
            Mnemonic     : String (1 .. 7);
         end record;
      Quick_Ops : constant array (Positive range <>) of Quick_Operation :=
        ((Integer_Constant (1),     Op_Add,      "inc    "),
         (Integer_Constant (not 0), Op_Add,      "dec    "),
         (Integer_Constant (1),     Op_Sub,      "dec    "),
         (Integer_Constant (not 0), Op_Sub,      "inc    "),
         (Integer_Constant (0),     Op_Nop,      "clr    "),
         (Integer_Constant (0),     Op_Compare,  "tst    "),
         (Integer_Constant (0),     Op_Mul,      "clr    "),
         (Integer_Constant (1),     Op_Mul,      "nop    "),
         (Integer_Constant (1),     Op_Div,      "nop    "),
         (Integer_Constant (1),     Op_Nop,      "clr    "),
         (Integer_Constant (2),     Op_Mul,      "asl #1,"),
         (Integer_Constant (4),     Op_Mul,      "asl #2,"),
         (Integer_Constant (8),     Op_Mul,      "asl #3,"),
         (Integer_Constant (16),    Op_Mul,      "asl #4,"),
         (Integer_Constant (32),    Op_Mul,      "asl #5,"),
         (Integer_Constant (64),    Op_Mul,      "asl #6,"),
         (Integer_Constant (128),   Op_Mul,      "asl #7,"),
         (Integer_Constant (2),     Op_Div,      "asr #1,"),
         (Integer_Constant (4),     Op_Div,      "asr #2,"),
         (Integer_Constant (8),     Op_Div,      "asr #3,"),
         (Integer_Constant (16),    Op_Div,      "asr #4,"),
         (Integer_Constant (32),    Op_Div,      "asr #5,"),
         (Integer_Constant (64),    Op_Div,      "asr #6,"),
         (Integer_Constant (128),   Op_Div,      "asr #7,"));

   begin

      if Op in Two_Argument_Operator and then Is_Constant (Source) then
         for I in Quick_Ops'Range loop
            if Quick_Ops (I).Op = Op and then
              Quick_Ops (I).Source_Value = Get_Value (Source)
            then
               Instruction
                 (Asm,
                  Trim (Quick_Ops (I).Mnemonic, Right),
                  Get_Size (Dest),
                  To_Dst (Dest));
               return;
            end if;
         end loop;

      end if;

      if Op = Op_Dereference then
         Instruction (Asm, "mov", Get_Size (Dest),
                      To_Dereferenced_String (Source),
                      To_Dst (Dest));
         return;
      end if;

      if Op = Op_Test then
         return;
      end if;

      if Is_Condition_Operand (Dest) then
         declare
            Src      : constant String := To_Src (Source);
            Mnemonic : constant String := Get_Mnemonic (Op);
         begin
            Instruction (Asm, Mnemonic, Get_Size (Source), Src);
         end;
      else
         declare
            Src      : constant String := To_Src (Source);
            Dst      : constant String := To_Dst (Dest);
            Mnemonic : constant String := Get_Mnemonic (Op);
         begin
            Instruction (Asm, Mnemonic, Get_Size (Dest), Src, Dst);
         end;
      end if;

   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     Tagatha.Transfers.Transfer_Operand;
      Source_2 : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
      Src_1    : constant String := To_Src (Source_1);
      Src_2    : constant String := To_Src (Source_2);
      Dst      : constant String := To_Dst (Dest);
      Mnemonic : constant String := Get_Mnemonic (Op);
      Reversed : constant Boolean := Get_Reversed (Op);
   begin
      if Reversed then
         Instruction (Asm, Mnemonic,
                      Tagatha.Transfers.Get_Size (Dest),
                      Src_2, Src_1, Dst);
      else
         Instruction (Asm, Mnemonic,
                      Tagatha.Transfers.Get_Size (Dest),
                      Src_1, Src_2, Dst);
      end if;
   end Operate;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (T      : in out Pdp32_Translator;
      Asm    : in out Assembly'Class;
      Line   : Positive;
      Column : Positive)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line (".source_position" & Positive'Image (Line)
                    & Positive'Image (Column));
   end Set_Location;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (T      : in out Pdp32_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean)
   is
      pragma Unreferenced (T);
   begin
      if Global then
         Asm.Put_Line (".export " & Name);
      end if;
      Asm.Put_Line (Name & ":");
      Asm.Put_Line ("    mov fp, -(sp)");
      Asm.Put_Line ("    mov sp, fp");
   end Start;

   ----------------------------
   -- To_Dereferenced_String --
   ----------------------------

   function To_Dereferenced_String
     (Item        : Tagatha.Transfers.Transfer_Operand)
     return String
   is
   begin
      return "@" & To_Src (Item);
   end To_Dereferenced_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item : Tagatha.Transfers.Transfer_Operand;
      Source : Boolean)
      return String
   is
      use Tagatha.Transfers;
      Deref : constant Boolean := Is_Dereferenced (Item);

      function Deref_Ampersand (S : String) return String
      is (if Deref then "@" & S else S);

      function Deref_Paren (S : String) return String
      is (if Deref then "(" & S & ")" else S);

   begin
      if Is_Constant (Item) then
         return Deref_Ampersand ("#" & To_String (Get_Value (Item), Item));
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
                  return Deref_Ampersand
                    (Image (Addr + Get_Slice_Octet_Offset (Item)) & "(fp)");
               elsif Is_Argument (Item) then
                  return Deref_Ampersand
                    (Image (Addr) & "(fp)");
               else
                  return Deref_Ampersand ("-" & Image (Addr) & "(fp)");
               end if;
            elsif Is_Argument (Item) then
               return Deref_Ampersand (Image (Addr) & "(fp)");
            else
               return Deref_Ampersand ("-" & Image (Addr) & "(fp)");
            end if;
         end;
      elsif Is_Result (Item) then
         return Deref_Paren (Result_Register);
      elsif Is_Stack (Item) then
         if Source then
            return Deref_Ampersand ("(sp)+");
         else
            return Deref_Ampersand ("-(sp)");
         end if;
      elsif Is_External (Item) then
         if Is_Immediate (Item) then
            return Deref_Ampersand ("#" & External_Name (Item));
         else
            return Deref_Paren (External_Name (Item));
         end if;
      elsif Is_Temporary (Item) then
         declare
            R : String :=
                  Positive'Image
                    (Tagatha.Temporaries.Get_Register
                       (Get_Temporary (Item)));
         begin
            R (1) := 'r';
            return Deref_Paren (R);
         end;
      elsif Is_Text (Item) then
         declare
            function Escape (S : String) return String;

            ------------
            -- Escape --
            ------------

            function Escape (S : String) return String is
               Index : constant Natural :=
                         Ada.Strings.Fixed.Index (S, """");
            begin
               if Index = 0 then
                  return S;
               else
                  return S (S'First .. Index) & """"
                    & Escape (S (Index + 1 .. S'Last));
               end if;
            end Escape;
         begin
            return """" & Escape (Get_Text (Item)) & """";
         end;
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
            return "r";
         when C_Equal =>
            return "eq";
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
   -- To_Suffix --
   ---------------

   function To_Suffix
     (Size : Tagatha_Size)
      return String
   is
   begin
      case Size is
         when Default_Size | Default_Integer_Size | Default_Address_Size =>
            return "";
         when Size_8 =>
            return ".1";
         when Size_16 =>
            return ".2";
         when Size_32 =>
            return "";
         when Size_64 =>
            return ".8";
      end case;
   end To_Suffix;

   ---------------
   -- Word_Size --
   ---------------

   overriding
   function Word_Size (T : Pdp32_Translator) return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_32;
   end Word_Size;

end Tagatha.Code.Pdp32;
