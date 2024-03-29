with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Temporaries;

package body Tagatha.Code.Pdp11 is

   Result_Register : constant String := "r0";
   Next_Label      : Positive := 1;

   function Register_Image
     (Index : Positive)
      return String;

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                      return String;

   function To_String (Op      : Condition_Operator;
                       Negated : Boolean)
                       return String;

   procedure Move (Asm       : in out Assembly'Class;
                   Source    : in     Tagatha.Transfers.Transfer_Operand;
                   Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move_Address
     (Asm       : in out Assembly'Class;
      Source    : in     Tagatha.Transfers.Transfer_Operand;
      Dest      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Dest     : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Source   : in     String;
                          Dest     : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Octet     : in     Boolean;
                          Source   : in     String;
                          Dest     : in     String);

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Octet     : in     Boolean;
                          Dest     : in     String);

   function Get_Mnemonic (Op : Tagatha_Operator) return String;

   function Get_Bits (Size : Tagatha_Size) return Natural
   is (case Size.Category is
          when Tagatha_Default_Size => 16,
          when Tagatha_Integer_Size => 16,
          when Tagatha_Address_Size => 16,
          when Tagatha_Floating_Point_Size => 32,
          when Tagatha_Custom_Size  => Size.Octets * 8);

   function Get_Octets (Size : Tagatha_Size) return Natural
   is (case Size.Category is
          when Tagatha_Default_Size        => 2,
          when Tagatha_Integer_Size        => 2,
          when Tagatha_Address_Size        => 2,
          when Tagatha_Floating_Point_Size => 4,
          when Tagatha_Custom_Size         => Size.Octets);

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

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   ------------
   -- Encode --
   ------------

   overriding
   procedure Encode (T    : in out Pdp11_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer)
   is
      use Tagatha.Transfers;
      use type Tagatha.Labels.Tagatha_Label;
      Label : Tagatha.Labels.Tagatha_Label := Get_Label (Item);
   begin
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
            if T.Operator in Condition_Operator
              and then Cond in C_Equal | C_Not_Equal
            then
               Asm.Put_Line ("    b" & To_String (T.Operator, Cond = C_Equal)
                             & " " & Show (Dest, '_'));
            else
               Asm.Put_Line ("    b" & To_String (Cond, T.Reverse_Test) &
                               " " & Show (Dest, '_'));
            end if;
            T.Reverse_Test := False;
            T.Operator := Op_Nop;
         end;
      elsif Is_Call (Item) then
         declare
            use Tagatha.Labels;
            Dest : constant Tagatha_Label := Get_Destination (Item);
            Args : constant Natural := Get_Argument_Count (Item);
         begin
            Asm.Put_Line
              ("    jsr pc," & Tagatha.Labels.Show (Dest, 'L'));
            if Args = 0 then
               null;
            elsif Args = 1 then
               Asm.Put_Line ("    tst (sp)+");
            else
               Asm.Put_Line ("    add #" & Positive'Image (Args * 2)
                             & ", sp");
            end if;
         end;

      elsif Is_Frame_Reservation (Item) then
         if Get_Reservation (Item) /= 0 then
            declare
               Word_Count  : constant Integer := Get_Reservation (Item) * 2;
               Reservation : constant String := Word_Count'Image;
            begin
               if Word_Count = 0 then
                  null;
               elsif Word_Count = 2 then
                  Asm.Put_Line ("    tst -(sp)");
               elsif Word_Count = -2 then
                  Asm.Put_Line ("    tst (sp)+");
               else
                  Asm.Put_Line
                    ("    "
                     & (if Word_Count > 0 then "sub" else "add")
                     & " #"
                     & Reservation (2 .. Reservation'Last)
                     & ", sp");
               end if;
            end;
         end if;
      elsif Is_Native (Item) then
         Asm.Put_Line ("    " & Get_Native_Text (Item));
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
            if Is_Constant_Zero (Get_Source_2 (Item)) then
               Instruction (Asm, "tst",
                            Get_Size (Get_Source_1 (Item)) = Size_8,
                            To_String (Get_Source_1 (Item), True));
            else
               Operate (Asm, Op_Compare,
                        Get_Source_1 (Item), Get_Source_2 (Item));
            end if;
         elsif Same_Operand (Get_Source_2 (Item), Get_Destination (Item)) then
            Operate (Asm, Get_Operator (Item), Get_Source_1 (Item),
                     Get_Destination (Item));
         elsif Get_Operator (Item) in Condition_Operator
           and then Is_Condition_Operand (Get_Destination (Item))
         then
            if Is_Constant_Zero (Get_Source_2 (Item)) then
               Instruction (Asm, "tst",
                            Get_Size (Get_Source_1 (Item)) = Size_8,
                            To_String (Get_Source_1 (Item), True));
            else
               Operate (Asm, Op_Compare,
                        Get_Source_1 (Item), Get_Source_2 (Item));
            end if;
            T.Operator := Get_Operator (Item);
         elsif Get_Operator (Item) in Condition_Operator then
            declare
               Op : constant Condition_Operator :=
                      Get_Operator (Item);
               Dst       : constant String :=
                             To_Dst (Get_Destination (Item));
               Dst_Octet : constant Boolean :=
                             Get_Bits (Get_Size (Get_Destination (Item)))
                             <= 8;
            begin
               Instruction (Asm, "clr", Dst_Octet, "r0");
               Operate (Asm, Op, Get_Source_1 (Item), Get_Source_2 (Item));
               Asm.Put_Line ("    b" & To_String (Op, True)
                             & Next_Label'Image);
               Instruction (Asm, "inc", Dst_Octet, "r0");
               Asm.Put_Line (Next_Label'Image & ":");
               Instruction (Asm, "mov", Dst_Octet, "r0", Dst);
               Next_Label := Next_Label + 1;
            end;
         else
            if not Same_Operand
              (Get_Source_1 (Item), Get_Destination (Item))
            then
               Move (Asm, Get_Source_1 (Item), Get_Destination (Item));
            end if;
            Operate (Asm, Get_Operator (Item),
                     Get_Source_2 (Item),
                     Get_Destination (Item));
         end if;
      end if;
   end Encode;

   ------------
   -- Finish --
   ------------

   overriding
   procedure Finish (T   : in out Pdp11_Translator;
                     Asm : in out Assembly'Class)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line ("    mov (sp)+, r5");
      Asm.Put_Line ("    rts pc");
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
         when Op_Mul | Op_Div | Op_Mod =>
            raise Constraint_Error with
              "no native multiplication/division on the pdp-11";
         when Op_And | Op_Or | Op_Xor | Op_Not =>
            raise Constraint_Error with
              "whoops, and/or/xor/not more complicated on pdp-11";
         when Op_Bit_Test =>
            return "bit";
         when Op_Bit_Clear =>
            return "bic";
         when Op_Bit_Set =>
            return "bis";
         when Op_Equal .. Op_Less_Equal =>
            return "cmp";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the pdp-11";
         when Op_Compare =>
            return "cmp";
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the pdp-11";
         when Op_Negate =>
            return "neg";
         when Op_Complement =>
            return "not";
         when Op_Test =>
            return "tst";
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
     (Translator : Pdp11_Translator;
      Data       : Tagatha_Data_Type)
      return Register_Range_Record
   is
   begin
      case Data is
         when Untyped_Data | Address_Data =>
            --  R1 .. R4
            --  R0 is used for passing arguments and returning results
            --  R5 is the frame pointer.
            --  R6 is the stack, R7 is the PC
            return (1, 4);
         when Floating_Point_Data =>
            --  AC1 .. AC3
            return (5, 7);
      end case;
   end Get_Register_Range;

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator return Translator'Class is
      Result : Pdp11_Translator;
   begin
      return Translator'Class (Result);
   end Get_Translator;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Source   : in     String;
                          Dest     : in     String)
   is
   begin
      Asm.Put_Line ("    " & Mnemonic & " " & Source & ", " & Dest);
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

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Octet    : in     Boolean;
                          Source   : in     String;
                          Dest     : in     String)
   is
   begin
      if Octet then
         Instruction (Asm, Mnemonic & "b", Source, Dest);
      else
         Instruction (Asm, Mnemonic, Source, Dest);
      end if;
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm      : in out Assembly'Class;
                          Mnemonic : in     String;
                          Octet     : in     Boolean;
                          Dest     : in     String)
   is
   begin
      if Octet then
         Instruction (Asm, Mnemonic & "b", Dest);
      else
         Instruction (Asm, Mnemonic, Dest);
      end if;
   end Instruction;

   -----------
   -- Label --
   -----------

   overriding
   procedure Label (T     : in out Pdp11_Translator;
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
      Transfer_Size : constant Tagatha_Size :=
                        (if Is_Null_Operand (Dest)
                         then Get_Size (Source)
                         else Get_Size (Dest));
   begin
      if Is_Null_Operand (Dest) then
         if Source = Stack_Operand then
            if Transfer_Size in Size_8 | Size_16
              | Default_Size | Default_Integer_Size | Default_Address_Size
            then
               Asm.Put_Line ("    tst (sp)+");
            elsif Transfer_Size = Size_32 then
               Asm.Put_Line ("    tst (sp)+");
               Asm.Put_Line ("    tst (sp)+");
            else
               Asm.Put_Line
                 ("    add #"
                  & Get_Octets (Transfer_Size)'Image
                  & ", sp");
            end if;
         end if;
      elsif Is_Condition_Operand (Dest) then
         if Transfer_Size = Size_8 then
            Instruction (Asm, "tstb",
                         To_String (Source, True));
         elsif Transfer_Size in Size_16
           | Default_Size | Default_Integer_Size | Default_Address_Size
         then
            Instruction (Asm, "tst",
                         To_String (Source, True));
         else
            raise Constraint_Error with
              "pdp11: cannot test size"
              & Get_Bits (Transfer_Size)'Image;
         end if;
      elsif Is_Indirect (Source) then
         Move_Address (Asm, Source, Dest);
      else
         if Transfer_Size = Size_8 then
            Instruction (Asm, "movb",
                         To_String (Source, True),
                         To_String (Dest, False));
         elsif Transfer_Size in Size_16
           | Default_Size | Default_Integer_Size | Default_Address_Size
         then
            Instruction (Asm, "mov", To_Src (Source), To_Dst (Dest));
         else
            for I in 0 .. Get_Octets (Transfer_Size) / 2 - 1 loop
               Instruction (Asm, "mov",
                            To_Src (Slice (Source, I, Size_16)),
                            To_Dst (Slice (Dest, I, Size_16)));
            end loop;
         end if;
      end if;
   end Move;

   ------------------
   -- Move_Address --
   ------------------

   procedure Move_Address
     (Asm       : in out Assembly'Class;
      Source    : in     Tagatha.Transfers.Transfer_Operand;
      Dest      : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
   begin
      if Is_Argument (Source) or else Is_Local (Source) then
         declare
            Addr : Tagatha_Integer;
         begin
            if Is_Argument (Source) then
               Addr := Tagatha_Integer (Get_Arg_Offset (Source) * 2 + 2);
            else
               Addr := Tagatha_Integer (Get_Local_Offset (Source) * 2);
            end if;
            Asm.Put_Line ("    mov r5," & To_Dst (Dest));
            Asm.Put_Line ("    "
                          & (if Is_Argument (Source) then "add" else "sub")
                          & " #" & Addr'Image & "," & To_Dst (Dest));
         end;
      else
         raise Constraint_Error with
           "can't move address of " & Tagatha.Transfers.Show (Source);
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
      Octet : constant Boolean :=
                Get_Bits (Get_Size (Dest)) = 8;
   begin
      if Get_Bits (Get_Size (Dest)) <= 16 then
         case Op is
            when Op_Negate =>
               Instruction (Asm, "neg", Octet, To_Dst (Dest));
            when Op_Complement =>
               Instruction (Asm, "not", Octet, To_Dst (Dest));
            when Op_Not =>
               Instruction (Asm, "not", Octet, To_Dst (Dest));
            when Op_Test =>
               Instruction (Asm, "tst", Octet, To_Dst (Dest));
            when Op_Dereference =>
               Instruction (Asm, "mov", Octet,
                            To_Dereferenced_String (Dest),
                            To_Dst (Dest));
         end case;
      else
         raise Constraint_Error with
           "Pdp11 cannot handle" & Get_Bits (Get_Size (Dest))'Image
           & " bit data (yet)";
      end if;

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
               Instruction (Asm, Quick_Ops (I).Mnemonic, To_Dst (Dest));
               return;
            end if;
         end loop;

         if Op = Op_Mul then
            if Is_Integer (Get_Value (Source)) then
               declare
                  Mul  : Tagatha_Integer := Get_Integer (Get_Value (Source));
                  Last : Positive        := 1;
                  Dst : constant String := To_Dst (Dest);
               begin
                  Instruction (Asm, "mov", "r0", "-(sp)");
                  Instruction (Asm, "mov", Dst, "r0");
                  Instruction (Asm, "clr", Dst);
                  for I in 1 .. Get_Bits (Get_Size (Dest)) loop
                     if Mul mod 2 = 1 then
                        if I > Last then
                           Instruction (Asm, "asl",
                                        "#" & Integer'Image (I - Last),
                                        "r0");
                           Last := I;
                        end if;
                        Instruction (Asm, "add", "r0", Dst);
                     end if;
                     Mul := Mul / 2;
                     exit when Mul = 0;
                  end loop;
                  Instruction (Asm, "mov", "(sp)+", "r0");
                  return;
               end;
            end if;
         end if;
      end if;

      if Op = Op_Dereference then
         Instruction (Asm, "mov", Get_Size (Dest) = Size_8,
                      To_Dereferenced_String (Source),
                      To_Dst (Dest));
         return;
      end if;

      if Op = Op_Div or else Op = Op_Mod then
         Asm.Put_Line ("    mov r0, -(sp)");
         Asm.Put_Line ("    mov r1, -(sp)");
         Instruction (Asm, "mov", To_Src (Source), "r1");
         Instruction (Asm, "mov", To_Dst (Dest), "r0");
         Asm.Put_Line ("    call sys__divide");
         if Op = Op_Div then
            Instruction (Asm, "mov", "r0", To_Dst (Dest));
         else
            Instruction (Asm, "mov", "r1", To_Dst (Dest));
         end if;
         return;
      end if;

      if Op = Op_Not then
         Instruction (Asm, "tst", To_Src (Source));
         Instruction (Asm, "beq", "+1");
         Instruction (Asm, "clr", To_Dst (Dest));
         Instruction (Asm, "beq", "+2");
         Asm.Put_Line ("1:  mov #1, " & To_Dst (Dest));
         Asm.Put_Line ("2:");
         return;
      end if;

      if Op = Op_Test then
         return;
      end if;

      declare
         Src      : constant String := To_Src (Source);
         Dst      : constant String := To_Dst (Dest);
         Mnemonic : constant String := Get_Mnemonic (Op);
         Octet     : constant Boolean :=
                       Get_Size (Dest) = Size_8
                         and then Mnemonic /= "add"
                             and then Mnemonic /= "sub";
      begin
         if Get_Data (Source) = Floating_Point_Data then
            Asm.Put_Line ("    ldf ac0," & Src);
            Asm.Put_Line ("    ldf ac1," & Dst);
            Asm.Put_Line ("    cmpf ac0,ac1");
         else
            if Get_Bits (Get_Size (Dest)) > 16 then
               raise Constraint_Error with
                 "Pdp11 cannot handle" & Get_Bits (Get_Size (Dest))'Image
                 & " bit data (yet)";
            end if;
            Instruction (Asm, Mnemonic, Octet, Src, Dst);
         end if;
      end;
   end Operate;

   --------------------
   -- Register_Image --
   --------------------

   function Register_Image
     (Index : Positive)
      return String
   is
   begin
      if Index <= 4 then
         return ('r', Character'Val (Index + 48));
      else
         return ('a', 'c', Character'Val (Index + 48 - 3));
      end if;
   end Register_Image;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (T      : in out Pdp11_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean)
   is
      pragma Unreferenced (T);
   begin
      if Global then
         Asm.Put_Line (".globl " & Name);
      end if;
      Asm.Put_Line (Name & ":");
      Asm.Put_Line ("    mov r5, -(sp)");
      Asm.Put_Line ("    mov sp, r5");
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

      function Eval return String;

      ----------
      -- Eval --
      ----------

      function Eval return String is
         Dereferenced : constant Boolean := Is_Dereferenced (Item);
      begin
         if Is_Constant (Item) then
            if Tagatha.Constants.Is_Label (Get_Value (Item)) then
               return "@#" & To_String (Get_Value (Item));
            else
               return (if Dereferenced then "@" else "")
                 & "#"
                 & To_String (Get_Value (Item), Item);
            end if;
         elsif Is_Argument (Item) or else Is_Local (Item) then
            declare
               Addr : Tagatha_Integer;
            begin
               if Is_Argument (Item) then
                  Addr := Tagatha_Integer (Get_Arg_Offset (Item) * 2 + 2);
               else
                  Addr := Tagatha_Integer (Get_Local_Offset (Item) * 2);
               end if;
               if Has_Slice (Item) then
                  if Slice_Fits (Item, Size_8) then
                     return Image
                       (Addr + Get_Slice_Octet_Offset (Item)) & "(r5)";
                  elsif Is_Argument (Item) then
                     return Image (Addr) & "(r5)";
                  else
                     return "-"
                       & Image (Addr - Get_Slice_Bit_Offset (Item) / 8)
                       & "(r5)";
                  end if;
               elsif Is_Argument (Item) then
                  return Image (Addr) & "(r5)";
               else
                  return "-" & Image (Addr) & "(r5)";
               end if;
            end;
         elsif Is_Result (Item) then
            return Result_Register;
         elsif Is_Stack (Item) then
            if Source then
               return "(sp)+";
            else
               return "-(sp)";
            end if;
         elsif Is_External (Item) then
            return (if Dereferenced then "@" else "")
              & (if Is_Immediate (Item) then "#" else "")
              & External_Name (Item);
         elsif Is_Temporary (Item) then
            declare
               R : constant String :=
                     Register_Image
                       (Tagatha.Temporaries.Get_Register
                          (Get_Temporary (Item)));
            begin
               return (if Dereferenced then '(' & R & ')' else R);
            end;
         else
            raise Constraint_Error with
              "unknown operand type in " & Show (Item);
         end if;
      end Eval;

   begin
      return Eval;
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
   -- To_String --
   ---------------

   function To_String (Op      : Condition_Operator;
                       Negated : Boolean)
                       return String
   is
      Cond : constant Tagatha_Condition :=
               (case Op is
                   when Op_Equal         => C_Equal,
                   when Op_Not_Equal     => C_Not_Equal,
                   when Op_Greater       => C_Greater,
                   when Op_Greater_Equal => C_At_Least,
                   when Op_Less          => C_Less,
                   when Op_Less_Equal    => C_At_Most);

   begin
      return To_String (Cond, Negated);
   end To_String;

   ---------------
   -- Word_Size --
   ---------------

   overriding
   function Word_Size (T : Pdp11_Translator) return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_16;
   end Word_Size;

end Tagatha.Code.Pdp11;
