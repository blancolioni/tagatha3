with Tagatha.Constants;

package body Tagatha.Fragments is

   ------------
   -- Append --
   ------------

   procedure Append (To_Fragment : in out Tagatha_Fragment;
                     Fragment    : in     Tagatha_Fragment)
   is
   begin
      To_Fragment.Records.Append (Fragment.Records);
   end Append;

   --------------------
   -- Append_To_Unit --
   --------------------

   procedure Append_To_Unit
     (Unit     : in out Tagatha.Units.Tagatha_Unit'Class;
      Fragment : in     Tagatha_Fragment)
   is
      Last_Reference : Natural := 0;
      Last_Condition : Natural := 0;
   begin
      for I in 1 .. Fragment.Records.Last_Index loop
         declare
            Rec : constant Fragment_Record := Fragment.Records.Element (I);
         begin
            if Rec.Fragment_Type /= Push_Fragment and then
              Rec.Fragment_Type /= Pop_Fragment and then
              Last_Reference /= 0
            then
               declare
                  Ref : constant Fragment_Record :=
                    Fragment.Records.Element (Last_Reference);
               begin
                  Last_Reference := 0;
                  Unit.Push_Operand (Ref.Reference, Ref.Size);
               end;
            end if;

            if Rec.Line /= 0 then
               Unit.Source_Location (Rec.Line, Rec.Column);
            end if;

            case Rec.Fragment_Type is
               when Command_Fragment =>
                  Unit.Command (Rec.Command);
               when Operand_Fragment =>
                  Last_Reference := I;
               when Condition_Fragment =>
                  Last_Condition := I;
               when Branch_Fragment =>
                  declare
                     Cond : Tagatha_Condition;
                  begin
                     if Last_Condition = 0 then
                        Cond := C_Always;
                     else
                        declare
                           Cond_Rec : constant Fragment_Record :=
                             Fragment.Records.Element (Last_Condition);
                        begin
                           Cond := Cond_Rec.Condition;
                        end;
                     end if;

                     if Rec.Branch_Condition or else Cond /= C_Always then
                        if not Rec.Branch_Condition then
                           Cond := Negate (Cond);
                        end if;

                        Unit.Jump (Rec.Branch_Target, Cond);

                     else

                        Unit.Jump (Rec.Branch_Target, C_Always);

                     end if;
                  end;

                  Last_Condition := 0;

               when Push_Fragment =>
                  if Last_Reference = 0 then
                     raise Constraint_Error with
                       "attempted to push without a reference";
                  else
                     declare
                        Ref : constant Fragment_Record :=
                          Fragment.Records.Element (Last_Reference);
                     begin
                        Last_Reference := 0;
                        Unit.Push_Operand (Ref.Reference, Ref.Size);
                     end;
                  end if;
               when Pop_Fragment =>
                  if Last_Reference = 0 then
                     raise Constraint_Error with
                       "attempted to pop without a reference";
                  else
                     declare
                        Ref : constant Fragment_Record :=
                          Fragment.Records.Element (Last_Reference);
                     begin
                        Last_Reference := 0;
                        Unit.Pop_Operand (Ref.Reference, Ref.Size);
                     end;
                  end if;
               when Label_Fragment =>
                  Unit.Label (Rec.Label_Index);
            end case;
         end;
      end loop;
   end Append_To_Unit;

   ------------
   -- Branch --
   ------------

   function Branch (Target           : Positive;
                    Branch_Condition : Boolean  := True)
                   return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type    => Branch_Fragment,
                              Line             => 0,
                              Column           => 0,
                              Branch_Target    => Target,
                              Branch_Condition => Branch_Condition));
      return Result;
   end Branch;

   ---------------------------
   -- Check_Source_Location --
   ---------------------------

   procedure Check_Source_Location
     (Fragment     : in out Tagatha_Fragment;
      Line, Column : Positive)
   is
   begin
      for I in 1 .. Fragment.Records.Last_Index loop
         declare
            Rec : Fragment_Record renames Fragment.Records (I);
         begin
            if Rec.Line = 0 then
               Rec.Line := Line;
               Rec.Column := Column;
            end if;
         end;
      end loop;
   end Check_Source_Location;

   -------------
   -- Command --
   -------------

   function Command (Cmd : Tagatha.Commands.Tagatha_Command)
                    return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Command_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Command       => Cmd));
      return Result;
   end Command;

   -------------
   -- Compare --
   -------------

   function Compare (Cond   : Tagatha_Condition;
                     Size   : Tagatha_Size := Default_Integer_Size)
                    return Tagatha_Fragment
   is
      Result : Tagatha_Fragment :=
        Operator (Op_Compare, Size => Size);
   begin
      Append (Result, Condition (Cond));
      return Result;
   end Compare;

   ---------------
   -- Condition --
   ---------------

   function Condition (Cond  : Tagatha_Condition) return Tagatha_Fragment is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Condition_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Condition     => Cond));
      return Result;
   end Condition;

   ----------------------
   -- Integer_Constant --
   ----------------------

   function Integer_Constant
     (Value : in Tagatha_Integer;
      Size  : in Tagatha_Size      := Default_Integer_Size)
     return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append
        ((Fragment_Type => Operand_Fragment,
          Line          => 0,
          Column        => 0,
          Reference     =>
            Tagatha.Transfers.Constant_Operand
              (Tagatha.Constants.Integer_Constant (Value)),
          Size          => Size));
      return Result;
   end Integer_Constant;

   -----------
   -- Label --
   -----------

   function Label
     (Index : Positive)
      return Tagatha_Fragment
   is
   begin
      return Result : Tagatha_Fragment do
         Result.Records.Append ((Label_Fragment, 0, 0, Index));
      end return;
   end Label;

   --------------
   -- Operator --
   --------------

   function Operator (Op     : Tagatha_Operator;
                      Negate : Boolean          := False;
                      Size   : Tagatha_Size     := Default_Integer_Size)
                     return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Command_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Command       =>
                                Tagatha.Commands.Operate (Op, Negate, Size)));
      return Result;
   end Operator;

   ---------
   -- Pop --
   ---------

   function Pop return Tagatha_Fragment is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Pop_Fragment, 0, 0));
      return Result;
   end Pop;

   ----------
   -- Push --
   ----------

   function Push return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Push_Fragment, 0, 0));
      return Result;
   end Push;

   ------------------------
   -- Reference_Argument --
   ------------------------

   function Reference_Argument
     (Offset   : in Argument_Offset;
      Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Operand_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Reference     =>
                                Tagatha.Transfers.Argument_Operand (Offset),
                              Size          => Size));
      return Result;
   end Reference_Argument;

   ------------------------
   -- Reference_External --
   ------------------------

   function Reference_External
     (Name      : String;
      Immediate : Boolean;
      Size      : Tagatha_Size  := Default_Integer_Size)
      return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Operand_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Reference     =>
                                Tagatha.Transfers.External_Operand
                                  (Name, Immediate),
                              Size          => Size));
      return Result;
   end Reference_External;

   ---------------------
   -- Reference_Local --
   ---------------------

   function Reference_Local
     (Offset   : in Local_Offset;
      Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Operand_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Reference     =>
                                Tagatha.Transfers.Local_Operand (Offset),
                              Size          => Size));
      return Result;
   end Reference_Local;

   ----------------------
   -- Reference_Result --
   ----------------------

   function Reference_Result
     (Size     : in Tagatha_Size  := Default_Integer_Size)
     return Tagatha_Fragment
   is
      Result : Tagatha_Fragment;
   begin
      Result.Records.Append ((Fragment_Type => Operand_Fragment,
                              Line          => 0,
                              Column        => 0,
                              Reference     =>
                                Tagatha.Transfers.Result_Operand,
                              Size          => Size));
      return Result;
   end Reference_Result;

   ----------
   -- Show --
   ----------

   function Show (Fragment : Tagatha_Fragment) return String is
      function S (Index : Positive) return String;
      function Show_Single (R : Fragment_Record) return String;

      function S (Index : Positive) return String is
      begin
         if Index = Fragment.Records.Last_Index then
            return Show_Single (Fragment.Records.Element (Index));
         else
            return Show_Single (Fragment.Records.Element (Index)) &
              ";" & S (Index + 1);
         end if;
      end S;

      -----------------
      -- Show_Single --
      -----------------

      function Show_Single (R : Fragment_Record) return String is
      begin
         case R.Fragment_Type is
            when Command_Fragment =>
               return Tagatha.Commands.Show (R.Command);
            when Operand_Fragment =>
               return Tagatha.Transfers.Show (R.Reference);
            when Condition_Fragment =>
               return Tagatha_Condition'Image (R.Condition);
            when Branch_Fragment =>
               declare
                  Label : constant String := "L" &
                            Integer'Image (-R.Branch_Target);
               begin
                  if R.Branch_Condition then
                     return "beq " & Label;
                  else
                     return "bne" & Label;
                  end if;
               end;
            when Pop_Fragment =>
               return "pop";
            when Push_Fragment =>
               return "push";
            when Label_Fragment =>
               declare
                  Result : String := Positive'Image (R.Label_Index);
               begin
                  Result (Result'First) := '$';
                  return Result;
               end;
         end case;
      end Show_Single;

   begin
      if Fragment.Records.Last_Index > 0 then
         return S (1);
      else
         return "<empty>";
      end if;
   end Show;

end Tagatha.Fragments;
