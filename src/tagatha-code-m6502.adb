with Tagatha.Constants;

package body Tagatha.Code.M6502 is

   type Temp_Address is
     (Div_Mul_Arg_1,
      Div_Mul_Arg_2,
      Div_Mul_Result_1,
      Div_Mul_Result_2);

   procedure Move_To_Temp_Address
     (T      : in out M6502_Translator'Class;
      Asm    : in out Assembly'Class;
      Temp   : in     Temp_Address;
      Source : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move_From_Temp_Address
     (T    : in out M6502_Translator'Class;
      Asm  : in out Assembly'Class;
      Temp : in     Temp_Address;
      Dest : in     Tagatha.Transfers.Transfer_Operand);

   procedure Instruction (Asm : in out Assembly'Class;
                          Text : String);

   procedure Instruction (Asm : in out Assembly'Class;
                          Mnemonic : String;
                          Argument : String);

   procedure Encode_Condition
     (Asm         : in out Assembly'Class;
      Cond        : Tagatha_Condition;
      Destination : Tagatha.Labels.Tagatha_Label;
      Negated     : Boolean);

   procedure Compare
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Source      : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand);

   procedure Move
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Source      : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand);

   procedure Multiply
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Operator    : in     Multiplication_Operator;
      Source_1    : in     Tagatha.Transfers.Transfer_Operand;
      Source_2    : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand);

   procedure Test
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Source      : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Start_Operands
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Src, Dst    : Tagatha.Transfers.Transfer_Operand);

   procedure Start_Operand
     (T    : in out M6502_Translator'Class;
      Asm  : in out Assembly'Class;
      Op   : Tagatha.Transfers.Transfer_Operand);

   procedure Next_Operand_Octet
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean);

   procedure Finish_Operand
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean);

   procedure Operate
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean;
      Octet        : Positive;
      Instr       : String);

   procedure Load_Register
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Register;
      Value       : Natural);

   procedure Decrement
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Index_Register);

   procedure Increment
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Index_Register);
   pragma Unreferenced (Increment);

   function Get_Offset
     (T  : M6502_Translator'Class;
      Op : Tagatha.Transfers.Transfer_Operand)
      return Natural;

   function Size_Octets (Size : Tagatha_Size) return Positive;

   ------------------
   -- Address_Size --
   ------------------

   overriding
   function Address_Size (T : M6502_Translator) return Tagatha_Size is
      pragma Unreferenced (T);
   begin
      return Size_16;
   end Address_Size;

   -------------
   -- Compare --
   -------------

   procedure Compare
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Source      : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand)
   is
      Source_Size : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Source));
      Dest_Size   : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Dest));
   begin
      Asm.Put_Line ("    ; compare " & Tagatha.Transfers.Show (Source)
                    & ", " & Tagatha.Transfers.Show (Dest));
      Start_Operands (T, Asm, Src => Source, Dst => Dest);

      for Octet in 1 .. Dest_Size loop
         if Octet <= Source_Size then
            Operate (T, Asm, Source, False, Octet, "LDA");
         elsif Octet = Source_Size + 1 then
            Instruction (Asm, "LDA", "#0");
         end if;
         Operate (T, Asm, Dest, True, Octet, "CMP");
         if Octet < Source_Size then
            Next_Operand_Octet (T, Asm, Source, False);
         end if;
         if Octet < Dest_Size then
            Next_Operand_Octet (T, Asm, Source, False);
         end if;
      end loop;
      Finish_Operand (T, Asm, Source, Destination => False);
      Finish_Operand (T, Asm, Dest, Destination => True);
   end Compare;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Index_Register)
   is
      R : Natural renames T.Registers (Reg).Value;
   begin
      Instruction (Asm, "DE" & Reg'Img);
      if T.Registers (Reg).Have_Value then
         if R = 0 then
            R := 255;
         else
            R := R - 1;
         end if;
      end if;
      if Reg = Y then
         T.Y_Is_FP := False;
      elsif Reg = X then
         T.X_Is_FP := False;
      end if;

   end Decrement;

   ------------
   -- Encode --
   ------------

   overriding procedure Encode
     (T    : in out M6502_Translator;
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

      Asm.Put_Line ("    ; ENCODE: " & Tagatha.Transfers.Show (Item));

      if Is_Control (Item) then
         declare
            use Tagatha.Labels;
            Cond : constant Tagatha_Condition := Get_Condition (Item);
            Dest : constant Tagatha_Label     := Get_Destination (Item);
         begin
            Encode_Condition (Asm, Cond, Dest, T.Reverse_Test);
            T.Reverse_Test := False;
         end;
      elsif Is_Frame_Reservation (Item) then
         --  PLA/PHA: 4 cycles
         --  TSX/TXS: 2 cycles
         --  INX/DEX: 2 cycles
         --  TXA, TAX: 2 cycles
         --  ADC, SBC (immediate): 2 cycles
         --  so we calculate the fastest method based on the number of
         --  slots to reserve
         --  Sequences are (Reservation > 0)
         --    (i)   PHA, ..., PHA
         --    (ii)  TSX, DEX, ..., TXS
         --    (iii) TSX, TXA, SEC, SBC, TAX, TXS
         --  (analogous sequences for Reservation < 0)
         --  Timings are (where r = abs Reservation)
         --    (i) 4 * r
         --    (ii) 4 + 2 * r
         --    (iii) 12
         --  Space used is
         --    (i)   r
         --    (ii)  r + 2
         --    (iii) 7
         --
         --  Therefore, assuming timing more important than space,
         --  sequence used is
         --    (i), if r <= 2
         --    (ii), if r <= 4
         --    (iii), if r > 4
         --
         --  Summary of timing and space
         --     Reservation:   1   2   3   4   5+
         --     Cycles:        4   8  10  12  12
         --     Octets:         1   2   5   6   7

         declare
            Reservation : constant Integer := Get_Reservation (Item);
            R           : constant Natural := abs Reservation;
            Subtract    : constant Boolean := Reservation > 0;
         begin
            case R is
               when 0 =>
                  null;
               when 1 | 2 =>
                  for I in 1 .. R loop
                     if Subtract then
                        Instruction (Asm, "PHA");
                     else
                        Instruction (Asm, "PLA");
                     end if;
                  end loop;
                  T.X_Is_FP := False;
               when 3 | 4 =>
                  Instruction (Asm, "TSX");
                  for I in 1 .. R loop
                     if Subtract then
                        Instruction (Asm, "DEX");
                     else
                        Instruction (Asm, "INX");
                     end if;
                  end loop;
                  Instruction (Asm, "TXS");
                  T.X_Is_FP := True;
               when others =>
                  Instruction (Asm, "TXS");
                  Instruction (Asm, "TXA");
                  if Subtract then
                     Instruction (Asm, "SEC");
                     Instruction (Asm, "SBC #" & Natural'Image (R));
                  else
                     Instruction (Asm, "CLC");
                     Instruction (Asm, "ADC #" & Natural'Image (R));
                  end if;
                  Instruction (Asm, "TAX");
                  Instruction (Asm, "TXS");
                  T.X_Is_FP := True;
            end case;

            T.Frame_Size := T.Frame_Size + Reservation;

            if not T.X_Is_FP then
               Instruction (Asm, "TSX");
            end if;

            Instruction (Asm, "STX", "FP");
            T.X_Is_FP := True;

         end;
      elsif Is_Simple (Item) then
         Move (T, Asm, Get_Source (Item), Get_Destination (Item));
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
               Test (T, Asm, Get_Source_1 (Item));
            else
               Compare (T, Asm,
                        Get_Source_1 (Item), Get_Source_2 (Item));
            end if;
         elsif Get_Operator (Item) in Multiplication_Operator then
            Multiply (T, Asm, Get_Operator (Item),
                      Get_Source_1 (Item), Get_Source_2 (Item),
                      Get_Destination (Item));
         elsif Same_Operand
           (Get_Source_2 (Item), Get_Destination (Item))
         then
            Asm.Put_Line ("    ; simplifying");
            Operate (Asm, Get_Operator (Item), Get_Source_1 (Item),
                     Get_Destination (Item));
         else
            Asm.Put_Line ("    ; moving");
            Move (T, Asm, Get_Source_1 (Item), Get_Destination (Item));
            Operate (Asm, Get_Operator (Item),
                     Get_Source_2 (Item),
                     Get_Destination (Item));
         end if;
      end if;
   end Encode;

   ----------------------
   -- Encode_Condition --
   ----------------------

   procedure Encode_Condition
     (Asm         : in out Assembly'Class;
      Cond        : Tagatha_Condition;
      Destination : Tagatha.Labels.Tagatha_Label;
      Negated     : Boolean)
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

      C : constant Tagatha_Condition := Actual_Condition;
      Dest : constant String := Tagatha.Labels.Show (Destination, '_');
   begin
      case C is
         when C_Always =>
            Instruction (Asm, "JMP", Dest);
         when C_Equal =>
            Instruction (Asm, "BEQ", Dest);
         when C_Not_Equal =>
            Instruction (Asm, "BNE", Dest);
         when C_Greater =>
            Asm.Put_Line (".(");
            Instruction (Asm, "BMI", "out");
            Instruction (Asm, "BNE", Dest);
            Asm.Put_Line ("out:");
            Asm.Put_Line (".)");
         when C_Less =>
            Instruction (Asm, "BMI", Dest);
         when C_At_Least =>
            Instruction (Asm, "BPL", Dest);
         when C_At_Most =>
            Instruction (Asm, "BMI", Dest);
            Instruction (Asm, "BEQ", Dest);
      end case;

   end Encode_Condition;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (T   : in out M6502_Translator;
      Asm : in out Assembly'Class)
   is
      pragma Unreferenced (T);
   begin
      Instruction (Asm, "RTS");
   end Finish;

   --------------------
   -- Finish_Operand --
   --------------------

   procedure Finish_Operand
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean)
   is
      pragma Unreferenced (T);
      pragma Unreferenced (Asm);
      pragma Unreferenced (Op);
      pragma Unreferenced (Destination);
   begin
      null;
   end Finish_Operand;

   -----------------------
   -- General_Registers --
   -----------------------

   overriding function General_Registers
     (T : M6502_Translator)
      return Positive
   is
      pragma Unreferenced (T);
   begin
      --  use zero page for registers
      --  let's use 4 of them
      return 4;
   end General_Registers;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset
     (T  : M6502_Translator'Class;
      Op : Tagatha.Transfers.Transfer_Operand)
      return Natural
   is
   begin
      if Transfers.Is_Local (Op) then
         return T.Frame_Size - Natural (Transfers.Get_Local_Offset (Op)) + 1;
      elsif Transfers.Is_Argument (Op) then
         --  add two for skipping the return address
         return T.Frame_Size + Natural (Transfers.Get_Arg_Offset (Op)) + 2;
      else
         raise Program_Error
           with "attempt to get offset from non-stack object";
      end if;
   end Get_Offset;

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator return Translator'Class is
      Result : M6502_Translator;
   begin
      return Translator'Class (Result);
   end Get_Translator;

   ---------------
   -- Increment --
   ---------------

   procedure Increment
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Index_Register)
   is
      R : Natural renames T.Registers (Reg).Value;
   begin
      Instruction (Asm, "IN" & Reg'Img);
      if T.Registers (Reg).Have_Value then
         if R = 255 then
            R := 0;
         else
            R := R + 1;
         end if;
      end if;
      if Reg = Y then
         T.Y_Is_FP := False;
      elsif Reg = X then
         T.X_Is_FP := False;
      end if;
   end Increment;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm : in out Assembly'Class;
                          Mnemonic : String;
                          Argument : String)
   is
   begin
      Instruction (Asm, Mnemonic & " " & Argument);
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction (Asm : in out Assembly'Class;
                          Text : String)
   is
   begin
      Asm.Put_Line ("    " & Text);
   end Instruction;

   ------------------
   -- Integer_Size --
   ------------------

   overriding
   function Integer_Size (T : M6502_Translator) return Tagatha_Size is
      pragma Unreferenced (T);
   begin
      return Size_8;
   end Integer_Size;

   -----------
   -- Label --
   -----------

   overriding procedure Label
     (T     : in out M6502_Translator;
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

   -------------------
   -- Load_Register --
   -------------------

   procedure Load_Register
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Reg         : Register;
      Value       : Natural)
   is
   begin
      if T.Registers (Reg).Have_Value
        and then T.Registers (Reg).Value = Value
      then
         return;
      end if;

      case Reg is
         when A | X | Y =>
            Instruction (Asm, "LD" & Reg'Img, "#" & Value'Img);
         when FP | A1 | A2 | A3 | A4 =>
            Instruction (Asm, "LDA", "#" & Value'Img);
            Instruction (Asm, "STA", Reg'Img);
            T.Registers (A) := (True, Value);
         when SP =>
            Instruction (Asm, "LDX", "#" & Value'Img);
            Instruction (Asm, "TXS");
            T.Registers (X) := (True, Value);
      end case;
      T.Registers (Reg) := (True, Value);
   end Load_Register;

   ----------
   -- Move --
   ----------

   procedure Move
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Source      : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand)
   is
      Source_Size : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Source));
      Dest_Size   : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Dest));
   begin
      Asm.Put_Line ("    ; move " & Tagatha.Transfers.Show (Source)
                    & ", " & Tagatha.Transfers.Show (Dest));
      Start_Operands (T, Asm, Src => Source, Dst => Dest);

      for Octet in 1 .. Dest_Size loop
         if Octet <= Source_Size then
            Operate (T, Asm, Source, False, Octet, "LDA");
         elsif Octet = Source_Size + 1 then
            Instruction (Asm, "LDA", "#0");
         end if;
         Operate (T, Asm, Dest, True, Octet, "STA");
         if Octet < Source_Size then
            Next_Operand_Octet (T, Asm, Source, False);
         end if;
         if Octet < Dest_Size then
            Next_Operand_Octet (T, Asm, Source, False);
         end if;
      end loop;
      Finish_Operand (T, Asm, Source, Destination => False);
      Finish_Operand (T, Asm, Dest, Destination => True);
   end Move;

   ----------------------------
   -- Move_From_Temp_Address --
   ----------------------------

   procedure Move_From_Temp_Address
     (T      : in out M6502_Translator'Class;
      Asm    : in out Assembly'Class;
      Temp   : in     Temp_Address;
      Dest   : in     Tagatha.Transfers.Transfer_Operand)
   is
      Dest_Size : constant Positive :=
                    Size_Octets (Transfers.Get_Size (Dest));
   begin
      Start_Operand (T, Asm, Dest);

      for Octet in 1 .. Dest_Size loop
         if Octet = 1 then
            Instruction (Asm, "LDA", Temp'Img);
         else
            Instruction (Asm, "LDA", Temp'Img & " +" & Octet'Img);
         end if;
         Operate (T, Asm, Dest, True, Octet, "STA");
      end loop;

      Finish_Operand (T, Asm, Dest, Destination => True);
   end Move_From_Temp_Address;

   --------------------------
   -- Move_To_Temp_Address --
   --------------------------

   procedure Move_To_Temp_Address
     (T      : in out M6502_Translator'Class;
      Asm    : in out Assembly'Class;
      Temp   : in     Temp_Address;
      Source : in     Tagatha.Transfers.Transfer_Operand)
   is
      Source_Size : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Source));
   begin
      Start_Operand (T, Asm, Source);

      for Octet in 1 .. Source_Size loop
         Operate (T, Asm, Source, False, Octet, "LDA");
         if Octet = 1 then
            Instruction (Asm, "STA", Temp'Img);
         else
            Instruction (Asm, "STA", Temp'Img & " +" & Octet'Img);
         end if;
      end loop;

      Finish_Operand (T, Asm, Source, Destination => False);
   end Move_To_Temp_Address;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Operator    : in     Multiplication_Operator;
      Source_1    : in     Tagatha.Transfers.Transfer_Operand;
      Source_2    : in     Tagatha.Transfers.Transfer_Operand;
      Dest        : in     Tagatha.Transfers.Transfer_Operand)
   is
      Dest_Size   : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Dest));
   begin
      Move_To_Temp_Address (T, Asm, Div_Mul_Arg_1, Source_1);
      Move_To_Temp_Address (T, Asm, Div_Mul_Arg_2, Source_2);

      case Operator is
         when Op_Mul =>
            if Dest_Size = 1 then
               Instruction (Asm, "JSR", "mul_8");
            elsif Dest_Size = 2 then
               Instruction (Asm, "JSR", "mul_16");
            else
               Instruction (Asm, "JSR", "mul_16");
            end if;
            Move_From_Temp_Address (T, Asm, Div_Mul_Result_1, Dest);

         when Op_Div | Op_Mod =>
            if Dest_Size = 1 then
               Instruction (Asm, "JSR", "div_8");
               T.Registers (X).Have_Value := False;
            elsif Dest_Size = 2 then
               Instruction (Asm, "JSR", "div_16");
            else
               Instruction (Asm, "JSR", "div_16");
            end if;
            if Operator = Op_Div then
               Move_From_Temp_Address (T, Asm, Div_Mul_Result_1, Dest);
            else
               Move_From_Temp_Address (T, Asm, Div_Mul_Result_2, Dest);
            end if;
      end case;

   end Multiply;

   -----------------------
   -- Next_Operand_Octet --
   -----------------------

   procedure Next_Operand_Octet
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean)
   is
      pragma Unreferenced (Destination);
      use Tagatha.Transfers;
   begin
      if Is_Constant (Op) then
         null;
      elsif Is_Argument (Op)
        or else Is_Local (Op)
      then
         if not T.Y_Decremented then
            Instruction (Asm, "DEY");
            T.Y_Decremented := True;
            Decrement (T, Asm, Y);
         else
            T.Y_Decremented := False;
         end if;
      end if;
   end Next_Operand_Octet;

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
      Asm.Put_Line ("    ; " & Op'Img
                    & " " & Tagatha.Transfers.Show (Source)
                    & ", " & Tagatha.Transfers.Show (Dest));
   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Asm      : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      Asm.Put_Line ("    ; " & Op'Img
                    & " " & Tagatha.Transfers.Show (Dest));
   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Op          : Tagatha.Transfers.Transfer_Operand;
      Destination : Boolean;
      Octet        : Positive;
      Instr       : String)
   is
      use Tagatha.Transfers;
   begin
      if Is_Constant (Op) then
         pragma Assert (not Destination);
         declare
            Value : constant Tagatha.Constants.Tagatha_Constant :=
                      Transfers.Get_Value (Op);
            Octet_Value : constant Natural :=
                           Constants.Get_Octet
                             (Value, Octet);
         begin
            Load_Register (T, Asm, A, Octet_Value);
         end;
      elsif Is_Argument (Op)
        or else Is_Local (Op)
      then
         declare
            Reg : constant Address_Register :=
                    (if Destination
                     then T.Dest_Addr
                     else T.Source_Addr);
         begin
            Instruction (Asm, Instr,
                         "(" & Register'Image (Reg) & "), Y");
            if not Destination then
               T.Registers (A).Have_Value := False;
            end if;
         end;
      end if;
   end Operate;

   ----------------
   -- Size_Octets --
   ----------------

   function Size_Octets (Size : Tagatha_Size) return Positive
   is (if Size in Default_Size | Default_Integer_Size | Size_8
       then 1
       elsif Size in Default_Address_Size | Size_16
       then 2
       else Size_Bits (Size) / 8);

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (T      : in out M6502_Translator;
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
   end Start;

   -------------------
   -- Start_Operand --
   -------------------

   procedure Start_Operand
     (T   : in out M6502_Translator'Class;
      Asm : in out Assembly'Class;
      Op  : Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Need_Reg  : constant Boolean :=
                      Is_Argument (Op) or else Is_Local (Op);
      Offset    : constant Natural :=
                      (if Need_Reg then Get_Offset (T, Op) else 0);
      Got_Reg   : Boolean := False;
   begin
      for R in Address_Register loop
         if T.Registers (R).Have_Value then
            if Need_Reg and then not Got_Reg
              and then T.Registers (R).Value = Offset
            then
               T.Source_Addr := R;
               Got_Reg := True;
               exit;
            end if;
         end if;
      end loop;

      if Need_Reg and then not Got_Reg then
         for R in Address_Register loop
            if not T.Registers (R).Have_Value then
               T.Source_Addr := R;
               Got_Reg := True;
               exit;
            end if;
         end loop;
      end if;

      if Need_Reg and then not Got_Reg then
         T.Source_Addr := A1;
         Got_Reg := True;
      end if;

      if Need_Reg
        and then (not T.Registers (T.Source_Addr).Have_Value
                  or else T.Registers (T.Source_Addr).Value /= Offset)
      then
         Load_Register (T, Asm, T.Source_Addr, Offset);
      end if;

      if Need_Reg then
         if not T.Y_Is_FP then
            Instruction (Asm, "LDY", "FP");
            T.Y_Is_FP := True;
         end if;
      end if;

   end Start_Operand;

   --------------------
   -- Start_Operands --
   --------------------

   procedure Start_Operands
     (T           : in out M6502_Translator'Class;
      Asm         : in out Assembly'Class;
      Src, Dst    : Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Need_Src_Reg : constant Boolean :=
                       Is_Argument (Src) or else Is_Local (Src);
      Need_Dst_Reg : constant Boolean :=
                       Is_Argument (Dst) or else Is_Local (Dst);
      Source_Offset : constant Natural :=
                        (if Need_Src_Reg then Get_Offset (T, Src) else 0);
      Dest_Offset   : constant Natural :=
                        (if Need_Dst_Reg then Get_Offset (T, Dst) else 0);
      Got_Src_Reg   : Boolean := False;
      Got_Dst_Reg   : Boolean := False;
   begin
      for R in Address_Register loop
         if T.Registers (R).Have_Value then
            if Need_Src_Reg and then not Got_Src_Reg
              and then T.Registers (R).Value = Source_Offset
            then
               T.Source_Addr := R;
               Got_Src_Reg := True;
            elsif Need_Dst_Reg and then not Got_Dst_Reg
              and then T.Registers (R).Value = Dest_Offset
            then
               T.Dest_Addr := R;
               Got_Dst_Reg := True;
            end if;
         end if;
      end loop;

      if (Need_Src_Reg and then not Got_Src_Reg)
        or else (Need_Dst_Reg and then not Got_Dst_Reg)
      then
         for R in Address_Register loop
            if not T.Registers (R).Have_Value then
               if Need_Src_Reg and then not Got_Src_Reg then
                  T.Source_Addr := R;
                  Got_Src_Reg := True;
                  exit when not Need_Dst_Reg;
               elsif Need_Dst_Reg and then not Got_Dst_Reg then
                  T.Dest_Addr := R;
                  Got_Dst_Reg := True;
                  exit;
               end if;
            end if;
         end loop;

         if Need_Src_Reg and then not Got_Src_Reg then
            T.Source_Addr := A1;
         end if;
         if Need_Dst_Reg and then not Got_Dst_Reg then
            T.Dest_Addr := A2;
         end if;
      end if;

      if Need_Src_Reg
        and then (not T.Registers (T.Source_Addr).Have_Value
                  or else T.Registers (T.Source_Addr).Value /= Source_Offset)
      then
         Load_Register (T, Asm, T.Source_Addr, Source_Offset);
      end if;

      if Need_Dst_Reg
        and then (not T.Registers (T.Dest_Addr).Have_Value
                  or else T.Registers (T.Dest_Addr).Value /= Dest_Offset)
      then
         Load_Register (T, Asm, T.Dest_Addr, Dest_Offset);
      end if;

      if Need_Src_Reg or else Need_Dst_Reg then
         if not T.Y_Is_FP then
            Instruction (Asm, "LDY", "FP");
            T.Y_Is_FP := True;
         end if;
      end if;

   end Start_Operands;

   ----------
   -- Test --
   ----------

   procedure Test
     (T      : in out M6502_Translator'Class;
      Asm    : in out Assembly'Class;
      Source : in     Tagatha.Transfers.Transfer_Operand)
   is
      Source_Size : constant Positive :=
                      Size_Octets (Transfers.Get_Size (Source));
   begin
      Asm.Put_Line ("    ; test " & Tagatha.Transfers.Show (Source));
      if Source_Size > 1 then
         Asm.Put_Line (".(");
      end if;

      Start_Operand (T, Asm, Source);

      for Octet in 1 .. Source_Size loop
         Operate (T, Asm, Source, False, Octet, "LDA");
         if Source_Size > 1 then
            Instruction (Asm, "BNE", "out");
            if Octet < Source_Size then
               Next_Operand_Octet (T, Asm, Source, False);
            end if;
         end if;
      end loop;

      Finish_Operand (T, Asm, Source, Destination => False);
      if Source_Size > 1 then
         Asm.Put_Line ("out:");
         Asm.Put_Line (".)");
      end if;
   end Test;

   ---------------
   -- Word_Size --
   ---------------

   overriding function Word_Size
     (T : M6502_Translator)
      return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_8;
   end Word_Size;

end Tagatha.Code.M6502;
