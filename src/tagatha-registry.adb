with Ada.Text_IO;

package body Tagatha.Registry is

   Trace_Registry : constant Boolean := False;

   function Pop
     (Register : in out Tagatha_Registry)
      return Expression_Record;

   procedure Push
     (Register   : in out Tagatha_Registry;
      Expression : Tagatha.Expressions.Expression;
      Label      : Tagatha.Labels.Tagatha_Label :=
        Tagatha.Labels.No_Label);

   procedure Append (Register : in out Tagatha_Registry;
                     T        : in     Tagatha.Transfers.Transfer);

   procedure Insert (Register : in out Tagatha_Registry;
                     Sequence : in     Positive;
                     Ts       : in     Tagatha.Transfers.Array_Of_Transfers);

   ------------
   -- Append --
   ------------

   procedure Append
     (Register : in out Tagatha_Registry;
      T        : in     Tagatha.Transfers.Transfer)
   is
      LT : Tagatha.Transfers.Transfer := T;
   begin
      if Labels.Has_Label (Register.Current_Label) then
         if Trace_Registry then
            Ada.Text_IO.Put_Line
              (Tagatha.Labels.Show_All (Register.Current_Label, 'L')
               & " " & Tagatha.Transfers.Show (LT));
         end if;

         Transfers.Set_Label (LT, Register.Current_Label);
         Register.Current_Label := Tagatha.Labels.No_Label;
      end if;

      if Register.Last_Line > 0 then
         Transfers.Set_Location
           (LT, Register.Last_Line, Register.Last_Column);
      end if;

      Register.Push_Index := Register.Push_Index + 1;
      Register.Transfers.Append ((LT, Register.Push_Index));
      if Trace_Registry then
         Ada.Text_IO.Put_Line
           ("append: transfer"
            & Positive'Image (Register.Transfers.Last_Index)
            & " <- " & Tagatha.Transfers.Show (LT));
      end if;
   end Append;

   -------------------
   -- Get_Transfers --
   -------------------

   procedure Get_Transfers
     (Register  : in Tagatha_Registry;
      Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector)
   is
      Reserve_Stack : constant Boolean := Register.Frame_Size > 0;
   begin
      Transfers.Clear;
      if Reserve_Stack then
         Transfers.Append
           (Tagatha.Transfers.Reserve_Stack
              (Register.Frame_Size));
      end if;

      for I in 1 .. Register.Transfers.Last_Index loop
         declare
            T : constant Tagatha.Transfers.Transfer :=
                  Register.Transfers (I).Transfer;
         begin
            Transfers.Append (T);
         end;
      end loop;

      if Labels.Has_Label (Register.Current_Label) then
         declare
            T : Tagatha.Transfers.Transfer :=
                  Tagatha.Transfers.Reserve_Stack (0);
         begin
            Tagatha.Transfers.Set_Label (T, Register.Current_Label);
            Transfers.Append (T);
         end;
      end if;

      if Reserve_Stack then
         Transfers.Append
           (Tagatha.Transfers.Restore_Stack (Register.Frame_Size));
      end if;

   end Get_Transfers;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Register : in out Tagatha_Registry;
      Sequence : in     Positive;
      Ts       : in     Tagatha.Transfers.Array_Of_Transfers)
   is
      use Tagatha.Transfers;

      Label       : Tagatha.Labels.Tagatha_Label :=
                      Tagatha.Labels.No_Label;

      procedure Make_Room
        (Start_Index : out Positive);

      procedure Copy (Arr   : Array_Of_Transfers;
                      Start : Positive);

      ----------
      -- Copy --
      ----------

      procedure Copy (Arr   : Array_Of_Transfers;
                      Start : Positive)
      is
         This_Index : Positive := Start;
         First      : Transfer := Arr (Arr'First);
         Line       : Positive := Register.Last_Line;
         Column     : Positive := Register.Last_Column;
      begin

         if Tagatha.Labels.Has_Label (Label) then
            Tagatha.Labels.Link_To (Label, Register.Current_Label);
         else
            Label := Register.Current_Label;
         end if;
         Register.Current_Label := Tagatha.Labels.No_Label;

         if Tagatha.Labels.Has_Label (Label) then
            if Trace_Registry then
               Ada.Text_IO.Put_Line
                 ("insert: "
                  & Tagatha.Labels.Show_All (Label, 'L')
                  & " at"
                  & Start'Img);
            end if;

            Tagatha.Transfers.Set_Label
              (First, Label);
         end if;

         Register.Current_Label := Tagatha.Labels.No_Label;

         for I in Arr'Range loop
            declare
               T : Transfer :=
                     (if I = Arr'First then First else Arr (I));
            begin
               if Transfers.Has_Location (T) then
                  Line := Transfers.Get_Line (T);
                  Column := Transfers.Get_Column (T);
               else
                  Transfers.Set_Location (T, Line, Column);
               end if;

               if Trace_Registry then
                  Ada.Text_IO.Put_Line
                    ("insert: transfer" & This_Index'Img
                     & " <- " & Show (T));
               end if;
               Register.Transfers.Replace_Element
                 (This_Index, (T, Sequence));
            end;
            This_Index := This_Index + 1;

         end loop;
      end Copy;

      ---------------
      -- Make_Room --
      ---------------

      procedure Make_Room
        (Start_Index : out Positive)
      is
         use Ada.Containers;
         Count           : Natural := 0;
      begin
         Start_Index := Register.Transfers.Last_Index + 1;
         Register.Transfers.Set_Length
           (Register.Transfers.Length
            + Ada.Containers.Count_Type (Ts'Length));
         while Start_Index > 1
           and then Register.Transfers
             (Start_Index - 1).Source_Index
           > Sequence
         loop
            Start_Index := Start_Index - 1;
            Count := Count + 1;
         end loop;
         for I in reverse 1 .. Count loop
            declare
               Target_Index : constant Positive :=
                                Start_Index + Ts'Length + I - 1;
               Source_Index : constant Positive :=
                                Start_Index + I - 1;
               Transfer     : Transfer_Record :=
                                Register.Transfers.Element
                                  (Source_Index);
            begin
               if Trace_Registry then
                  Ada.Text_IO.Put_Line
                    ("move: transfer"
                     & Positive'Image (Target_Index)
                     & " <- "
                     & Show (Transfer.Transfer));
               end if;

               if I = 1 then
                  if Trace_Registry
                    and then Tagatha.Labels.Has_Label
                      (Tagatha.Transfers.Get_Label (Transfer.Transfer))
                  then
                     Ada.Text_IO.Put_Line
                       ("move: saving label: "
                        & Tagatha.Labels.Show_All
                          (Tagatha.Transfers.Get_Label (Transfer.Transfer),
                          'L'));
                  end if;

                  Label := Tagatha.Transfers.Get_Label (Transfer.Transfer);
                  Tagatha.Transfers.Clear_Label (Transfer.Transfer);
               end if;

               Register.Transfers.Replace_Element
                 (Target_Index, Transfer);
            end;
         end loop;
      end Make_Room;

      Start_Index : Positive;
   begin
      Make_Room (Start_Index);
      Copy (Ts, Start_Index);
   end Insert;

   ---------
   -- Pop --
   ---------

   function Pop
     (Register : in out Tagatha_Registry)
      return Expression_Record
   is
      E : Expression_Record;
   begin
      if Register.Stack.Is_Empty then
         E :=
           (Tagatha.Expressions.New_Simple_Expression
              (Tagatha.Transfers.Stack_Operand),
            Register.Push_Index,
            Tagatha.Labels.No_Label);
         Register.Push_Index := Register.Push_Index + 1;
      else
         E := Register.Stack.Last_Element;
         Register.Stack.Delete_Last;
      end if;
      return E;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Register   : in out Tagatha_Registry;
      Expression : Tagatha.Expressions.Expression;
      Label      : Tagatha.Labels.Tagatha_Label :=
        Tagatha.Labels.No_Label)
   is
      use Tagatha.Labels;
   begin
      if Has_Label (Label) then
         Tagatha.Labels.Link_To (Label, Register.Current_Label);
         Register.Current_Label := Label;
      end if;
      Register.Push_Index := Register.Push_Index + 1;
      Register.Stack.Append
        ((Expression, Register.Push_Index, Label));
   end Push;

   -----------------
   -- Record_Call --
   -----------------

   procedure Record_Call
     (Register       : in out Tagatha_Registry;
      Subroutine     : in     Tagatha.Labels.Tagatha_Label;
      Argument_Count : Natural)
   is
   begin
      for Operand of Register.Stack loop
         declare
            Index     : constant Positive := Operand.Transfer_Index;
            Label     : constant Tagatha.Labels.Tagatha_Label :=
                          Operand.Label;
            Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps, Operand.Expression,
                             Tagatha.Transfers.Stack_Operand);
         begin
            Tagatha.Transfers.Set_Label
              (Transfers (Transfers'First), Label);
            Register.Insert (Index, Transfers);
         end;
      end loop;

      Register.Stack.Clear;

      Register.Append
        (Tagatha.Transfers.Call (Subroutine, Argument_Count));

   end Record_Call;

   -----------------
   -- Record_Drop --
   -----------------

   procedure Record_Drop (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size)
   is
   begin
      Record_Pop (Register, Size, Tagatha.Transfers.No_Operand);
   end Record_Drop;

   ----------------------
   -- Record_Duplicate --
   ----------------------

   procedure Record_Duplicate
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size)
   is
      T : constant Tagatha.Transfers.Transfer_Operand :=
            Tagatha.Transfers.Temporary_Operand
              (Tagatha.Temporaries.Next_Temporary (Register.Temps));
   begin
      Record_Pop (Register, Size, T);
      Record_Push (Register, Size, T);
      Record_Push (Register, Size, T);
   end Record_Duplicate;

   -----------------
   -- Record_Jump --
   -----------------

   procedure Record_Jump (Register    : in out Tagatha_Registry;
                          Condition   : in     Tagatha_Condition;
                          Destination : in     Tagatha.Labels.Tagatha_Label)
   is
   begin
      if Condition /= C_Always then
         Record_Pop (Register, Default_Address_Size,
                     Tagatha.Transfers.Condition_Operand);
      end if;

      for Operand of Register.Stack loop
         declare
            Index     : constant Positive := Operand.Transfer_Index;
            Label     : constant Tagatha.Labels.Tagatha_Label :=
                          Operand.Label;
            Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps, Operand.Expression,
                             Tagatha.Transfers.Stack_Operand);
         begin
            Tagatha.Transfers.Set_Label
              (Transfers (Transfers'First), Label);
            Register.Insert (Index, Transfers);
         end;
      end loop;

      Register.Stack.Clear;

      Register.Append
        (Tagatha.Transfers.Control_Transfer (Condition, Destination));

   end Record_Jump;

   ------------------
   -- Record_Label --
   ------------------

   procedure Record_Label
     (Register   : in out Tagatha_Registry;
      Label      : in     Tagatha.Labels.Tagatha_Label)
   is
      use type Tagatha.Labels.Tagatha_Label;
   begin
      if Label /= Tagatha.Labels.No_Label then
         for Operand of Register.Stack loop
            declare
               Index     : constant Positive := Operand.Transfer_Index;
               Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                             Tagatha.Expressions.Get_Transfers
                               (Register.Temps, Operand.Expression,
                                Tagatha.Transfers.Stack_Operand);
            begin
               Tagatha.Transfers.Set_Label
                 (Transfers (Transfers'First), Operand.Label);
               Register.Insert (Index, Transfers);
            end;
         end loop;
         Register.Stack.Clear;
         Tagatha.Labels.Link_To (Label, Register.Current_Label);
         Register.Current_Label := Label;
      end if;
   end Record_Label;

   ---------------------
   -- Record_Location --
   ---------------------

   procedure Record_Location (Register : in out Tagatha_Registry;
                              Line     : in     Positive;
                              Column   : in     Positive)
   is
   begin
      Register.Last_Line := Line;
      Register.Last_Column := Column;
   end Record_Location;

   -----------------
   -- Record_Loop --
   -----------------

   procedure Record_Loop (Register   : in out Tagatha_Registry;
                          Limit      : in     Local_Offset;
                          Counter    : in     Local_Offset;
                          End_Label  : in     Tagatha.Labels.Tagatha_Label)
   is
      pragma Unreferenced (Register);
      pragma Unreferenced (Limit);
      pragma Unreferenced (Counter);
      pragma Unreferenced (End_Label);
   begin
      null;
   end Record_Loop;

   -----------------------------
   -- Record_Native_Operation --
   -----------------------------

   procedure Record_Native_Operation
     (Register          : in out Tagatha_Registry;
      Name              : String;
      Changed_Registers : String;
      Input_Words       : Natural;
      Output_Words      : Natural)
   is
      pragma Unreferenced (Input_Words);
   begin
      for Operand of Register.Stack loop
         declare
            Index     : constant Positive := Operand.Transfer_Index;
            Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps, Operand.Expression,
                             Tagatha.Transfers.Stack_Operand);
         begin
            Tagatha.Transfers.Set_Label
              (Transfers (Transfers'First), Operand.Label);
            Register.Insert (Index, Transfers);
         end;
      end loop;

      Register.Stack.Clear;

      Register.Append
        (Tagatha.Transfers.Native_Transfer (Name, Changed_Registers));

      for I in 1 .. Output_Words loop
         Register.Push
           (Expressions.New_Simple_Expression (Transfers.Stack_Operand));
      end loop;
   end Record_Native_Operation;

   ----------------------
   -- Record_Operation --
   ----------------------

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator)
   is
      use Tagatha.Expressions;
   begin
      if Trace_Registry then
         Ada.Text_IO.Put_Line ("record operator: " & Operator'Img);
      end if;

      if Operator in Zero_Argument_Operator then
         null;
      elsif Operator in One_Argument_Operator then
         declare
            Left : constant Expression_Record := Pop (Register);
         begin
            Register.Push
              (New_Operator_Expression (Operator, Left.Expression),
               Left.Label);
         end;
      else
         declare
            Right : constant Expression_Record := Pop (Register);
            Left  : constant Expression_Record := Pop (Register);
            Ls    : Tagatha.Labels.Tagatha_Label := Left.Label;
         begin
            if not Tagatha.Labels.Has_Label (Ls) then
               Ls := Right.Label;
            elsif Tagatha.Labels.Has_Label (Right.Label) then
               Tagatha.Labels.Link_To (Ls, Right.Label);
            end if;

            if Trace_Registry then
               Ada.Text_IO.Put_Line
                 ("   push: "
                  & Tagatha.Labels.Show_All (Ls, 'L')
                  & ": "
                  & Tagatha.Expressions.Image
                    (New_Operator_Expression
                      (Operator, Left.Expression, Right.Expression)));
            end if;

            Register.Push
              (New_Operator_Expression
                 (Operator, Left.Expression, Right.Expression),
               Ls);
         end;
      end if;
   end Record_Operation;

   ----------------
   -- Record_Pop --
   ----------------

   procedure Record_Pop
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size;
      Operand  : in     Transfers.Transfer_Operand)
   is
      Transfer : Transfers.Transfer_Operand := Operand;
   begin
      if Trace_Registry then
         Ada.Text_IO.Put_Line ("Record_Pop: "
                               & Transfers.Show (Operand));
      end if;

      if Tagatha.Transfers.Is_Shelf (Operand) then
         declare
            Name : constant String :=
                     Tagatha.Transfers.Get_Shelf_Name (Operand);
            T    : constant Tagatha.Temporaries.Temporary :=
                     Tagatha.Temporaries.Next_Temporary (Register.Temps);
         begin
            if Name = "_" then
               Register.Default_Shelf := T;
            else
               if Register.Named_Shelves.Contains (Name) then
                  Register.Named_Shelves.Replace (Name, T);
               else
                  Register.Named_Shelves.Insert (Name, T);
               end if;
            end if;
            Transfer :=
              Tagatha.Transfers.Temporary_Operand (T);
         end;
      end if;

      if Register.Stack.Is_Empty then
         Register.Append
           (Tagatha.Transfers.Simple_Transfer
              (From => Tagatha.Transfers.Stack_Operand,
               To   => Transfer));
      else
         declare
            Element   : constant Expression_Record :=
                          Register.Stack.Element
                            (Register.Stack.Last_Index);
            Transfers : Tagatha.Transfers.Array_Of_Transfers :=
                          Tagatha.Expressions.Get_Transfers
                            (Register.Temps, Element.Expression,
                             Transfer);
         begin
            if Labels.Has_Label (Element.Label) then
               if Trace_Registry then
                  Ada.Text_IO.Put_Line
                    ("Record_Pop: "
                     & Tagatha.Transfers.Show (Transfer)
                     & Labels.Show_All (Element.Label, 'L')
                     & " -> "
                     & Tagatha.Transfers.Show (Transfers (Transfers'First)));
               end if;
            end if;
            if Transfers'Length > 0 then
               Tagatha.Transfers.Set_Size
                 (Transfers (Transfers'Last), Size);
               Tagatha.Transfers.Set_Label
                 (Transfers (Transfers'First), Element.Label);
               Register.Insert (Element.Transfer_Index, Transfers);
            end if;

            Register.Stack.Delete (Register.Stack.Last_Index);
         end;
      end if;
   end Record_Pop;

   -----------------
   -- Record_Push --
   -----------------

   procedure Record_Push (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size;
                          Operand  : in     Tagatha.Transfers.Transfer_Operand)
   is
      pragma Unreferenced (Size);
      use Tagatha.Expressions;
      Transfer : Tagatha.Transfers.Transfer_Operand := Operand;
   begin
      Register.Push_Index := Register.Push_Index + 1;
      if Trace_Registry then
         Ada.Text_IO.Put_Line
           ("record push at" & Integer'Image (Register.Push_Index) & ": "
            & Tagatha.Labels.Show_All (Register.Current_Label, 'L')
            & Tagatha.Transfers.Show (Operand));
      end if;
      if Tagatha.Transfers.Is_Shelf (Operand) then
         declare
            Name : constant String :=
                     Tagatha.Transfers.Get_Shelf_Name (Operand);
         begin
            if Name = "_" then
               Transfer :=
                 Tagatha.Transfers.Temporary_Operand (Register.Default_Shelf);
            else
               Transfer :=
                 Tagatha.Transfers.Temporary_Operand
                   (Register.Named_Shelves.Element (Name));
            end if;
         end;
      end if;

      Register.Push
        (New_Simple_Expression (Transfer));
   end Record_Push;

   ------------------
   -- Record_Store --
   ------------------

   procedure Record_Store
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size)
   is
      T_Address : constant Tagatha.Temporaries.Temporary :=
                    Tagatha.Temporaries.Next_Temporary
                      (Register.Temps);
      T_Value   : constant Tagatha.Temporaries.Temporary :=
                    Tagatha.Temporaries.Next_Temporary
                      (Register.Temps);
      Src_1     : constant Tagatha.Transfers.Transfer_Operand :=
                    Tagatha.Transfers.Temporary_Operand
                      (T_Address);
      Src_2     : constant Tagatha.Transfers.Transfer_Operand :=
                    Tagatha.Transfers.Temporary_Operand
                      (T_Value);
      Dst       : constant Tagatha.Transfers.Transfer_Operand :=
                    Tagatha.Transfers.Temporary_Operand
                      (T_Address, Indirect => True);
   begin
      Record_Pop (Register, Default_Address_Size, Src_1);
      Record_Pop (Register, Size, Src_2);
      Register.Append
        (Tagatha.Transfers.Simple_Transfer
           (From => Src_2,
            To   => Dst));
      if Trace_Registry then
         Ada.Text_IO.Put_Line
           ("Record_Store");
      end if;
   end Record_Store;

   -----------------
   -- Record_Swap --
   -----------------

   procedure Record_Swap
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size)
   is
      pragma Unreferenced (Size);
      Top : Expression_Record :=
              Register.Stack.Element
                (Register.Stack.Last_Index);
      Penultimate : Expression_Record :=
                      Register.Stack.Element
                        (Register.Stack.Last_Index - 1);
      T : constant Tagatha.Expressions.Expression := Top.Expression;
   begin
      if Trace_Registry then
         Ada.Text_IO.Put_Line
           ("swap: old stack ["
            & Expressions.Image
              (Register.Stack.Element
                   (Register.Stack.Last_Index).Expression)
            & "] ["
            & Expressions.Image
              (Register.Stack.Element
                   (Register.Stack.Last_Index - 1).Expression)
            & "]");
      end if;

      Top.Expression := Penultimate.Expression;
      Penultimate.Expression := T;
      Register.Stack.Replace_Element
        (Register.Stack.Last_Index, Top);
      Register.Stack.Replace_Element
        (Register.Stack.Last_Index - 1, Penultimate);

      if Trace_Registry then
         Ada.Text_IO.Put_Line
           ("swap: new stack ["
            & Expressions.Image
              (Register.Stack.Element
                   (Register.Stack.Last_Index).Expression)
            & "] ["
            & Expressions.Image
              (Register.Stack.Element
                   (Register.Stack.Last_Index - 1).Expression)
            & "]");
      end if;

   end Record_Swap;

   -----------
   -- Start --
   -----------

   procedure Start (Register    : in out Tagatha_Registry;
                    Unit_Label  : in     Tagatha.Labels.Tagatha_Label;
                    Size        : in     Natural)
   is
   begin
      Register.Unit_Label := Unit_Label;
      Register.Frame_Size := Size;
      Register.Temps := Tagatha.Temporaries.New_Source;
   end Start;

end Tagatha.Registry;
