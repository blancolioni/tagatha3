with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Temporaries;

package body Tagatha.Code.Pdp32 is

   Result_Register  : constant String := "r0";
   Address_Register : constant String := "r28";

   Last_Line : Natural := 0;
   Last_Col  : Natural := 0;

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                      return String;

   procedure Move (Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move_Address
     (Asm       : in out Assembly'Class;
      Source    : in     Tagatha.Transfers.Transfer_Operand;
      Dest      : in     String);

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

   -----------------
   -- Begin_Frame --
   -----------------

   overriding procedure Begin_Frame
     (T           : in out Pdp32_Translator;
      Asm         : in out Assembly'Class;
      Return_Count    : Natural;
      Arg_Count       : Natural;
      Local_Count     : Natural;
      Temporary_Count : Natural)
   is
      pragma Unreferenced (T, Arg_Count, Local_Count,
                           Return_Count, Temporary_Count);
   begin
      Asm.Put_Line ("    mov fp, -(sp)");
      Asm.Put_Line ("    mov sp, fp");
   end Begin_Frame;

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
         if Tagatha.Labels.Exported (Label) then
            Asm.Put_Line (".export " & Tagatha.Labels.Show (Label, 'L'));
         end if;
         Asm.Put_Line (Tagatha.Labels.Show (Label, 'L') & ":");
         Label := Tagatha.Labels.Next_Linked_Label (Label);
      end loop;

      if Is_Control (Item) then
         declare
            use Tagatha.Labels;
            Cond : constant Tagatha_Condition := Get_Condition (Item);
            Dest : constant Tagatha_Label     := Get_Destination (Item);
         begin
            if not Is_Local (Dest) then
               pragma Assert (Cond = C_Always and then not T.Reverse_Test);
               Asm.Put_Line ("    jmp " & Show (Dest, '!'));
            else
               Asm.Put_Line ("    b" & To_String (Cond, T.Reverse_Test) &
                               " " & Show (Dest, 'L'));
            end if;
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

   overriding procedure End_Frame
     (T           : in out Pdp32_Translator;
      Asm         : in out Assembly'Class;
      Arg_Count   : in     Natural;
      Local_Count : in     Natural)
   is
      pragma Unreferenced (T, Arg_Count, Local_Count);
   begin
      Asm.Put_Line ("    mov fp, sp");
      Asm.Put_Line ("    mov (sp)+, fp");
   end End_Frame;

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
      Last_Line := 0;
      Last_Col := 0;
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
      Asm.Put_Line ("    rts");
   end Finish;

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
         when Op_Logical_Shift =>
            return "lsh";
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

   ------------------------
   -- Get_Register_Range --
   ------------------------

   overriding function Get_Register_Range
     (Translator : Pdp32_Translator;
      Data       : Tagatha_Data_Type)
      return Register_Range_Record
   is
   begin
      case Data is
         when Untyped_Data | Address_Data =>
            return (1, 6);
         when Floating_Point_Data =>
            return (7, 10);
      end case;
   end Get_Register_Range;

   ------------------
   -- Get_Reversed --
   ------------------

   function Get_Reversed (Op : Tagatha_Operator) return Boolean is
      pragma Unreferenced (Op);
   begin
      return False; --  Op = Op_Sub;
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
      if Mnemonic = "neg" then
         Asm.Put_Line
           ("    mov" & To_Suffix (Size)
            & " " & Source & ", " & Dest);
         Asm.Put_Line
           ("    " & Mnemonic & To_Suffix (Size)
            & " " & Dest);
      else
         Asm.Put_Line
           ("    " & Mnemonic & To_Suffix (Size)
            & " " & Source & ", " & Dest);
      end if;
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
      if Mnemonic = "lsh" or else Mnemonic = "ash"
        or else Mnemonic = "bit"
        or else Mnemonic = "bic" or else Mnemonic = "bis"
      then
         Asm.Put_Line
           ("    mov" & To_Suffix (Size)
            & " " & Source_1 & ", " & Dest);
         Asm.Put_Line
           ("    " & Mnemonic & To_Suffix (Size)
            & " " &  Source_2 & ", " & Dest);
      else
         Asm.Put_Line
           ("    " & Mnemonic & To_Suffix (Size)
            & " " & Source_1 & ", " & Source_2 & ", " & Dest);
      end if;
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
         if Tagatha.Labels.Exported (L) then
            Asm.Put_Line (".export " & Tagatha.Labels.Show (L, 'L'));
         end if;
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
      Transfer_Size : constant Tagatha_Size :=
                        (if Is_Null_Operand (Dest)
                         then Get_Size (Source)
                         else Get_Size (Dest));
   begin

      if Is_Null_Operand (Dest) then
         if Source = Stack_Operand then
            if Transfer_Size in
              Size_8 | Size_16 | Size_32
                | Default_Size
                  | Default_Integer_Size | Default_Address_Size
            then
               Asm.Put_Line ("    tst (sp)+");
            else
               Asm.Put_Line
                 ("    add #"
                  & Size_Octets (Transfer_Size)'Image
                  & ", sp");
            end if;
         end if;
      elsif Is_Condition_Operand (Dest) then
         Instruction (Asm, "tst", Transfer_Size, To_Src (Source));
      elsif (Is_Constant (Source) or else Is_Immediate (Source))
        and then Is_Dereferenced (Source)
      then
         Instruction (Asm, "mov", Transfer_Size,
                      To_Src (Source),
                      "r0");
         Instruction (Asm, "mov", Transfer_Size,
                      "r0", To_Dst (Dest));
      elsif Is_Dereferenced (Source) and then Is_Stack (Source) then
         Instruction (Asm, "mov", Default_Address_Size,
                      "(sp)+", Address_Register);
         Instruction (Asm, "mov", Transfer_Size,
                      "(" & Address_Register & ")", To_Dst (Dest));
      elsif Is_Indirect (Source) then
         Move_Address (Asm, Source, Address_Register);
         Instruction (Asm, "mov", Transfer_Size, Address_Register,
                      To_Dst (Dest));
      else
         Instruction (Asm, "mov", Transfer_Size,
                      To_Src (Source),
                      To_Dst (Dest));
      end if;
   end Move;

   ------------------
   -- Move_Address --
   ------------------

   procedure Move_Address
     (Asm       : in out Assembly'Class;
      Source    : in     Tagatha.Transfers.Transfer_Operand;
      Dest      : in     String)
   is
      use Tagatha.Transfers;
   begin
      if Is_Argument (Source) then
         declare
            Offset : constant Tagatha_Integer :=
                       Tagatha_Integer (Get_Arg_Offset (Source)) * 4 + 4;
         begin
            Instruction (Asm, "add", Default_Address_Size,
                         "#" & Image (Offset), "fp", Dest);
         end;
      elsif Is_Local (Source) then
         declare
            Offset : constant Tagatha_Integer :=
                       Tagatha_Integer (Get_Local_Offset (Source)) * 4;
         begin
            Instruction (Asm, "sub", Default_Address_Size,
                         "#" & Image (Offset), "fp", Dest);
         end;
      else
         raise Constraint_Error with
           "bad operand for move address: " & Show (Source);
      end if;
   end Move_Address;

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
            if Is_Stack (Dest) then
               Instruction (Asm, "mov", Get_Size (Dest),
                            To_Src (Dest), Address_Register);
               Instruction (Asm, "mov", Get_Size (Dest),
                            "(" & Address_Register & ")",
                            To_Dst (Dest));
            else
               Instruction (Asm, "mov", Size,
                            To_Dereferenced_String (Dest),
                            To_Dst (Dest));
            end if;
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

      if Op = Op_Not then
         Instruction (Asm, "seq #0,", Get_Size (Dest),
                      To_Src (Source), To_Dst (Dest));
         return;
      end if;

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
         declare
            Src : constant String := To_Src (Source);
         begin
            if Src (Src'First) = '#'
              or else Is_Stack (Source)
            then
               Instruction (Asm, "mov", Get_Size (Dest),
                            Src, Address_Register);
               Instruction (Asm, "mov", Get_Size (Dest),
                            "(" & Address_Register & ")",
                            To_Dst (Dest));
            else
               Instruction (Asm, "mov", Get_Size (Dest),
                            To_Dereferenced_String (Source),
                            To_Dst (Dest));
            end if;
         end;
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
      elsif (Is_Constant (Source) or else Is_Immediate (Source))
        and then Is_Dereferenced (Source)
      then
         Instruction (Asm, "mov", Get_Size (Dest),
                      To_Src (Source), "r0");
         Instruction (Asm, Get_Mnemonic (Op),
                      Get_Size (Dest), "(r0)", To_Dst (Dest));
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
      if Line /= Last_Line or else Column /= Last_Col then
         Asm.Put_Line (".source_position" & Positive'Image (Line)
                       & Positive'Image (Column));
         Last_Line := Line;
         Last_Col := Column;
      end if;
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
   end Start;

   ----------------------------
   -- To_Dereferenced_String --
   ----------------------------

   function To_Dereferenced_String
     (Item        : Tagatha.Transfers.Transfer_Operand)
     return String
   is
      Base : constant String := To_Src (Item);
   begin
      if Base (Base'First) = '#'
        or else Ada.Strings.Fixed.Index (Base, "(") > 0
      then
         return "@" & Base;
      else
         return "(" & Base & ")";
      end if;
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

      function Predec return String
      is (if Has_Predecrement (Item) then "-" else "");

      function Postinc return String
      is (if Has_Postincrement (Item) then "+" else "");

   begin
      if Is_Constant (Item) then
         return "#" & To_String (Get_Value (Item), Item);
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
      elsif Is_Result (Item) or else Is_Return (Item) then
         return Deref_Paren (Result_Register);
      elsif Is_Stack (Item) then
         if Source then
            if Deref then
               return Deref_Ampersand ("(sp)+");
            else
               return Deref_Ampersand ("(sp)+");
            end if;
         else
            return Deref_Ampersand ("-(sp)");
         end if;
      elsif Is_External (Item) then
         if Is_Immediate (Item) then
            return "#" & External_Name (Item);
         else
            return Predec & Deref_Paren (External_Name (Item)) & Postinc;
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
      elsif Is_Iterator_New (Item) then
         return "agg";
      elsif Is_Iterator_Copy (Item) then
         return "(agg)+";
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
      if Size in
        Default_Size | Default_Integer_Size | Default_Address_Size
      then
         return "";
      elsif Size = Size_8 then
         return ".1";
      elsif Size = Size_16 then
         return ".2";
      elsif Size = Size_32 then
         return "";
      elsif Size = Size_64 then
         return ".8";
      else
         raise Constraint_Error with "invalid size for suffix";
      end if;
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
