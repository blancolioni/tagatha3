with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Temporaries;

package body Tagatha.Code.Aqua32 is

   Base_Register : constant String := "$255";

   Last_Line : Natural := 0;
   Last_Col  : Natural := 0;

   function To_String (Cond    : Tagatha_Condition;
                       Negated : Boolean)
                      return String;

   function Register_Image (Index : Natural) return String;

   function Argument_Register_Image
     (Translator : Aqua32_Translator'Class;
      Index      : Positive)
      return String
   is (Register_Image (Translator.Arg_Count - Index));

   function Result_Register_Image
     (Translator : Aqua32_Translator'Class;
      Index      : Positive)
      return String
   is (Register_Image (Translator.Arg_Count + Index - 1));

   function Jump_Register_Image
     (Translator : Aqua32_Translator'Class)
      return String
   is (Register_Image (Translator.Arg_Count + Translator.Ret_Count));

   function Local_Register_Image
     (Translator : Aqua32_Translator'Class;
      Index      : Positive)
      return String
   is (Register_Image (Translator.Arg_Count + Translator.Ret_Count + 1
                       + Index - 1));

   function Zero_Operand return Tagatha.Transfers.Transfer_Operand
   is (Tagatha.Transfers.Constant_Operand
       (Tagatha.Constants.Integer_Constant (0)));

   function Register_Operand
     (Register : String)
      return Tagatha.Transfers.Transfer_Operand
   is (Tagatha.Transfers.External_Operand
       (Register, False, False, False));

   function Scratch_Register_Image
     (Translator : Aqua32_Translator'Class;
      Index      : Positive)
      return String
   is (Register_Image (Translator.Ret_Count
                       + Translator.Arg_Count
                       + 1
                       + Translator.Local_Count
                       + Index));

   function Stack_Register_Image
     (Translator : Aqua32_Translator'Class)
      return String
   is (Register_Image (Translator.Ret_Count
                       + Translator.Arg_Count
                       + Translator.Local_Count
                       + 1
                       + Translator.Temp_Count
                       + Translator.Stack_Count.all - 1));

   procedure Move (Translator : Aqua32_Translator'Class;
                   Asm        : in out Assembly'Class;
                   Source     : in     Tagatha.Transfers.Transfer_Operand;
                   Dest       : in     Tagatha.Transfers.Transfer_Operand);

--     procedure Instruction (Translator : Aqua32_Translator'Class;
--                            Asm        : in out Assembly'Class;
--                            Mnemonic : in     String;
--                            Size     : in     Tagatha_Size;
--                            Source   : in     String;
--                            Dest     : in     String);

   function Get_Mnemonic (Op : Tagatha_Operator) return String;
   function Get_Reversed (Op : Tagatha_Operator) return Boolean;

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op       : in     Tagatha_Operator;
      Source_1 : in     Tagatha.Transfers.Transfer_Operand;
      Source_2 : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand);

   procedure Pop_Register
     (Translator : Aqua32_Translator'Class);

   procedure Push_Register
     (Translator : Aqua32_Translator'Class);

   procedure Change_Register_Stack
     (Translator : Aqua32_Translator'Class;
      Change     : Integer);

   function Pop_Register
     (Translator : Aqua32_Translator'Class)
      return String;

   function Push_Register
     (Translator : Aqua32_Translator'Class)
      return String;

   function With_Size
     (Name : String;
      Size : Tagatha_Size)
      return String
   is (Name &
       (case Size is
           when Default_Size => "W",
           when Default_Integer_Size => "W",
           when Default_Address_Size => "W",
           when Size_32              => "W",
           when Size_16              => "H",
           when Size_8               => "B",
           when Size_64              =>
              raise Constraint_Error with "64 bit not supported yet"));

   -----------------
   -- Begin_Frame --
   -----------------

   overriding procedure Begin_Frame
     (T               : in out Aqua32_Translator;
      Asm             : in out Assembly'Class;
      Return_Count    : Natural;
      Arg_Count       : Natural;
      Local_Count     : Natural;
      Temporary_Count : Natural)
   is
   begin
      T.Ret_Count := Return_Count;
      T.Arg_Count := Arg_Count;
      T.Local_Count := Local_Count;
      T.Temp_Count := Temporary_Count + 3;
      T.Stack_Count.all := 0;
--        Ada.Text_IO.Put_Line
--          ("begin frame:" & T.Ret_Count'Img & T.Arg_Count'Img
--           & T.Local_Count'Img & T.Temp_Count'Img);
      Asm.Put_Line ("    PUT rJ, "
                    & T.Jump_Register_Image);
   end Begin_Frame;

   ---------------------------
   -- Change_Register_Stack --
   ---------------------------

   procedure Change_Register_Stack
     (Translator : Aqua32_Translator'Class;
      Change     : Integer)
   is
      S : Integer renames Translator.Stack_Count.all;
   begin
      S := S + Change;
   end Change_Register_Stack;

   ------------
   -- Encode --
   ------------

   overriding procedure Encode
     (Translator : in out Aqua32_Translator;
      Asm        : in out Assembly'Class;
      Item       : in     Tagatha.Transfers.Transfer)
   is
      use Tagatha.Transfers;
      use type Tagatha.Labels.Tagatha_Label;
      Label : Tagatha.Labels.Tagatha_Label := Get_Label (Item);
   begin

--        Ada.Text_IO.Put_Line
--          ("encoding: " & Tagatha.Transfers.Show (Item));

      Asm.Put_Line
        ("--  " & Tagatha.Transfers.Show (Item));

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
            Asm.Put_Line ("    B"
                          & To_String (Cond, Translator.Reverse_Test) &
                            " " & Show (Dest, 'L'));
            Translator.Reverse_Test := False;
         end;
      elsif Is_Call (Item) then
         declare
            use Tagatha.Labels;
            Dest : constant Tagatha_Label := Get_Destination (Item);
            Preserve : constant Natural :=
                         Translator.Ret_Count
                         + Translator.Arg_Count
                         + Translator.Local_Count + Translator.Temp_Count
                         + Translator.Stack_Count.all
                         - Get_Argument_Count (Item) - 1;
         begin
            if Dest = No_Label then
               Asm.Put_Line
                 ("    PUSHGO" & Natural'Image (Preserve)
                  & ","
                  & Base_Register
                  & ","
                  & Translator.Pop_Register);
            else
               Asm.Put_Line
                 ("    PUSHJ" & Natural'Image (Preserve)
                  & ","
                  & Tagatha.Labels.Show (Dest, 'L'));
            end if;

            Translator.Change_Register_Stack
              (-Get_Argument_Count (Item) + 1);
--              Asm.Put_Line
--                ("    mov " & Register_Image (Preserve)
--                 & "," & Translator.Push_Register);
         end;
      elsif Is_Frame_Reservation (Item) then
         null;
--           Translator.Change_Register_Stack (Get_Reservation (Item));
      elsif Is_Native (Item) then
         Asm.Put_Line ("    " & Get_Native_Text (Item));
      elsif Is_Simple (Item) then
         Translator.Move (Asm, Get_Source (Item), Get_Destination (Item));
      elsif Get_Operator (Item) = Op_Test then
         null;
      elsif Get_Operator (Item) in One_Argument_Operator then
         if Same_Operand (Get_Source (Item), Get_Destination (Item)) then
            Translator.Operate
              (Asm, Get_Operator (Item),
               Get_Destination (Item));
         else
            Translator.Operate
              (Asm, Get_Operator (Item),
               Get_Source (Item), Get_Destination (Item));
         end if;
      else
         if Get_Operator (Item) = Op_Compare then
            Translator.Operate
              (Asm, Op_Compare,
               Get_Source_1 (Item), Get_Source_2 (Item));
         elsif Same_Operand (Get_Source_2 (Item), Get_Destination (Item))
           and then not Is_Stack (Get_Destination (Item))
         then
            Translator.Operate
              (Asm, Get_Operator (Item), Get_Source_1 (Item),
               Get_Destination (Item));
         else
            Translator.Operate
              (Asm, Get_Operator (Item),
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

   ---------------
   -- End_Frame --
   ---------------

   overriding procedure End_Frame
     (T           : in out Aqua32_Translator;
      Asm         : in out Assembly'Class;
      Arg_Count   : in     Natural;
      Local_Count : in     Natural)
   is
      pragma Unreferenced (Arg_Count, Local_Count);
   begin
      Asm.Put_Line
        ("    GET rJ, "
         & T.Jump_Register_Image);
   end End_Frame;

   -------------------
   -- File_Preamble --
   -------------------

   overriding procedure File_Preamble
     (T                : in out Aqua32_Translator;
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
   procedure Finish (T   : in out Aqua32_Translator;
                     Asm : in out Assembly'Class)
   is
   begin
      if T.Ret_Count > 0 and then T.Arg_Count > 0 then
         for I in 1 .. T.Ret_Count loop
            Asm.Put_Line
              ("    ADD " & T.Result_Register_Image (I) & ","
               & Register_Image (I) & ", 0");
         end loop;
      end if;

      Asm.Put_Line
        ("    POP" & Positive'Image (T.Ret_Count + 1));
      T.Ret_Count := 0;
      T.Arg_Count := 0;
      T.Local_Count := 0;
      T.Stack_Count.all := 0;

   end Finish;

   -----------------------
   -- General_Registers --
   -----------------------

   overriding
   function General_Registers (T : Aqua32_Translator) return Positive is
      pragma Unreferenced (T);
   begin
      return 100;
   end General_Registers;

   ------------------
   -- Get_Mnemonic --
   ------------------

   function Get_Mnemonic (Op : Tagatha_Operator) return String is
   begin
      case Op is
         when Op_Nop =>
            return "SWYM";
         when Op_Add =>
            return "ADD";
         when Op_Sub =>
            return "SUB";
         when Op_Mul =>
            return "MUL";
         when Op_Div =>
            return "DIV";
         when Op_Mod =>
            return "MOD";
         when Op_And =>
            return "AND";
         when Op_Or =>
            return "OR";
         when Op_Xor =>
            return "XOR";
         when Op_Not =>
            return "NOT";
         when Op_Bit_Test =>
            return "AND";
         when Op_Bit_Clear =>
            return "ANDN";
         when Op_Bit_Set =>
            return "OR";
         when Op_Equal =>
            return "CMP";
         when Op_Not_Equal =>
            return "CMP";
         when Op_Greater =>
            return "CMP";
         when Op_Less =>
            return "CMP";
         when Op_Greater_Equal =>
            return "CMP";
         when Op_Less_Equal =>
            return "CMP";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the aqua-32";
         when Op_Compare =>
            return "CMP";
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the aqua-32";
         when Op_Negate =>
            return "NEG";
         when Op_Complement =>
            return "NOT";
         when Op_Test =>
            return "CMP";
         when Op_Logical_Shift =>
            return "SL";
         when Op_Dereference =>
            raise Constraint_Error with
              "should not be getting a mnemonic for dereference";
      end case;
   end Get_Mnemonic;

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
      Result : Aqua32_Translator;
   begin
      Result.Stack_Count := new Natural'(0);
      return Translator'Class (Result);
   end Get_Translator;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Mnemonic   : in     String;
      Dest       : in     Tagatha.Transfers.Transfer_Operand;
      Source_1   : in     Tagatha.Transfers.Transfer_Operand;
      Source_2   : in     Tagatha.Transfers.Transfer_Operand)
   is
   begin
      Asm.Put_Line
        ("    " & Mnemonic
         & " "
         & Translator.To_Dst (Dest)
         & ","
         & Translator.To_Src (Source_1)
         & ","
         & Translator.To_Src (Source_2));
   end Instruction;

   -----------
   -- Label --
   -----------

   overriding
   procedure Label
     (Translator : in out Aqua32_Translator;
      Asm        : in out Assembly'Class;
      Label      : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (Translator);
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

   procedure Move (Translator : Aqua32_Translator'Class;
                   Asm        : in out Assembly'Class;
                   Source     : in     Tagatha.Transfers.Transfer_Operand;
                   Dest       : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Transfer_Size : Tagatha_Size := Size_32;
   begin
      if Has_Size (Dest) then
         Transfer_Size := Get_Size (Dest);
      end if;

      if Is_Null_Operand (Dest) then
         if Source = Stack_Operand then
            Translator.Pop_Register;
         end if;
      elsif Is_Condition_Operand (Dest) then
         Asm.Put_Line
           ("    CMP " & Translator.To_Dst (Dest)
            & ", "
            & Translator.To_Src (Source)
            & ", 0");
      elsif (Is_Constant (Source) or else Is_Immediate (Source))
        and then Is_Dereferenced (Source)
      then
         Asm.Put_Line
           ("    "
            & With_Size ("LD", Transfer_Size)
            & " "
            & Translator.Scratch_Register_Image (1)
            & ", "
            & Base_Register
            & ", "
            & Translator.To_Src (Source));
         Asm.Put_Line
           ("    LDW "
            & Translator.To_Dst (Dest)
            & ", "
            & Translator.Scratch_Register_Image (1)
            & ", "
            & "0");
      elsif Is_Constant (Source) or else Is_Immediate (Source) then
         Asm.Put_Line
           ("    SET " & Translator.To_Dst (Dest)
            & "," & Translator.To_Src (Source));
      else
         declare
            Src : constant String := Translator.To_Src (Source);
            Dst : constant String := Translator.To_Dst (Dest);
            function Simple (X : String) return Boolean
            is (X'Length >= 2
                and then X (X'First) = '$'
                and then (for all Ch of X (X'First + 1 .. X'Last) =>
                             Ch in '0' .. '9'));

         begin
            if Src = Dst
              and then Simple (Src)
            then
               null;  --  do not generate "mov rX, rX"
            else
               Asm.Put_Line
                 ("    ADD "
                  & Dst
                  & ","
                  & Src
                  & ","
                  & "0");
            end if;
         end;
      end if;
   end Move;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op       : in     One_Argument_Operator;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
      Size : constant Tagatha_Size := Get_Size (Dest);
   begin
      case Op is
         when Op_Negate =>
            Translator.Instruction
              (Asm      => Asm,
               Mnemonic => "NEG",
               Dest     => Dest,
               Source_1 => Zero_Operand,
               Source_2 => Dest);
         when Op_Complement | Op_Not =>
            Translator.Instruction
              (Asm, "NAND", Dest, Dest, Dest);
         when Op_Test =>
            Translator.Instruction
              (Asm      => Asm,
               Mnemonic => "CMP",
               Dest     =>
                  Register_Operand (Translator.Scratch_Register_Image (1)),
               Source_1 => Dest,
               Source_2 => Zero_Operand);
         when Op_Dereference =>
            Translator.Instruction
              (Asm      => Asm,
               Mnemonic => With_Size ("LD", Size),
               Dest     => Dest,
               Source_1 => Dest,
               Source_2 => Zero_Operand);
      end case;

   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op         : in     Tagatha_Operator;
      Source   : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
      use Tagatha.Transfers;
   begin

      if Op = Op_Not then
         Translator.Instruction
           (Asm      => Asm,
            Mnemonic => "CMP",
            Dest     => Dest,
            Source_1 => Source,
            Source_2 => Zero_Operand);
         return;
      end if;

      if Op = Op_Dereference
        or else ((Is_Constant (Source) or else Is_Immediate (Source))
                 and then Is_Dereferenced (Source))
      then
         declare
            Src : constant String := Translator.To_Src (Source);
         begin
            if Src (Src'First) = '#' then
               Asm.Put_Line
                 ("    " & With_Size ("LD", Get_Size (Dest))
                  & " "
                  & Translator.To_Dst (Dest)
                  & ","
                  & Base_Register
                  & ","
                  & Src);
            else
               Asm.Put_Line
                 ("    " & With_Size ("LD", Get_Size (Dest))
                  & " "
                  & Translator.To_Dst (Dest)
                  & "," & Src
                  & ",0");
            end if;
         end;
         return;
      end if;

      if Op = Op_Test then
         return;
      end if;

      Translator.Instruction
        (Asm      => Asm,
         Mnemonic => Get_Mnemonic (Op),
         Dest     => Dest,
         Source_1 => Source,
         Source_2 => Dest);

   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Translator : Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Op         : in     Tagatha_Operator;
      Source_1 : in     Tagatha.Transfers.Transfer_Operand;
      Source_2 : in     Tagatha.Transfers.Transfer_Operand;
      Dest     : in     Tagatha.Transfers.Transfer_Operand)
   is
      Mnemonic : constant String := Get_Mnemonic (Op);
      Reversed : constant Boolean := Get_Reversed (Op);
   begin
      if Reversed then
         Translator.Instruction
           (Asm      => Asm,
            Mnemonic => Mnemonic,
            Dest     => Dest,
            Source_1 => Source_2,
            Source_2 => Source_1);
      else
         Translator.Instruction
           (Asm      => Asm,
            Mnemonic => Mnemonic,
            Dest     => Dest,
            Source_1 => Source_1,
            Source_2 => Source_2);
      end if;
   end Operate;

   ------------------
   -- Pop_Register --
   ------------------

   procedure Pop_Register
     (Translator : Aqua32_Translator'Class)
   is
      S : Integer renames Translator.Stack_Count.all;
   begin
      S := S - 1;
   end Pop_Register;

   ------------------
   -- Pop_Register --
   ------------------

   function Pop_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is
   begin
      return Img : constant String := Translator.Stack_Register_Image do
         Translator.Pop_Register;
      end return;
   end Pop_Register;

   -------------------
   -- Push_Register --
   -------------------

   procedure Push_Register
     (Translator : Aqua32_Translator'Class)
   is
      S : Integer renames Translator.Stack_Count.all;
   begin
      S := S + 1;
   end Push_Register;

   -------------------
   -- Push_Register --
   -------------------

   function Push_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is
   begin
      Translator.Push_Register;
      return Translator.Stack_Register_Image;
   end Push_Register;

   --------------------
   -- Register_Image --
   --------------------

   function Register_Image (Index : Natural) return String is
      Result : String := Natural'Image (Index);
   begin
      Result (Result'First) := '$';
      return Result;
   end Register_Image;

   ------------------
   -- Set_Location --
   ------------------

   overriding procedure Set_Location
     (T      : in out Aqua32_Translator;
      Asm    : in out Assembly'Class;
      Line   : Positive;
      Column : Positive)
   is
      pragma Unreferenced (T);
   begin
      if Column /= Last_Col or else Line /= Last_Line then
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
   procedure Start (T      : in out Aqua32_Translator;
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

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Translator : Aqua32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand;
      Source     : Boolean)
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
      elsif Is_Argument (Item) then
         return Deref_Paren
           (Translator.Argument_Register_Image
              (Positive (Get_Arg_Offset (Item))));
      elsif Is_Local (Item) then
         return Deref_Paren
           (Translator.Local_Register_Image
              (Positive (Get_Local_Offset (Item))));
      elsif Is_Result (Item) then
         return Deref_Paren (Translator.Result_Register_Image (1));
      elsif Is_Stack (Item) then
         if Source then
            return Deref_Ampersand
              (Translator.Pop_Register);
         else
            return Deref_Ampersand (Translator.Push_Register);
         end if;
      elsif Is_External (Item) then
         if Is_Immediate (Item) then
            return "#" & External_Name (Item);
         else
            return Predec & Deref_Paren (External_Name (Item)) & Postinc;
         end if;
      elsif Is_Temporary (Item) then
         return Deref_Paren
           (Translator.Scratch_Register_Image
              (Tagatha.Temporaries.Get_Register
                   (Get_Temporary (Item))));
      elsif Is_Iterator_New (Item) then
         return Translator.Scratch_Register_Image
           (Translator.Temp_Count - 2);
      elsif Is_Iterator_Copy (Item) then
         return "("
           & Translator.Scratch_Register_Image
           (Translator.Temp_Count - 2)
           & ")+";
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
            return "R";
         when C_Equal =>
            return "Z";
         when C_Not_Equal =>
            return "NZ";
         when C_Greater =>
            return "P";
         when C_Less =>
            return "N";
         when C_At_Least =>
            return "NN";
         when C_At_Most =>
            return "NP";
      end case;
   end To_String;

   ---------------
   -- Word_Size --
   ---------------

   overriding
   function Word_Size (T : Aqua32_Translator) return Tagatha_Size
   is
      pragma Unreferenced (T);
   begin
      return Size_32;
   end Word_Size;

end Tagatha.Code.Aqua32;
