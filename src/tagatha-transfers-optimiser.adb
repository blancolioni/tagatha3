with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

package body Tagatha.Transfers.Optimiser is

   package Known_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Tagatha.Transfers.Transfer_Operand,
        Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   --------------
   -- Optimise --
   --------------

   procedure Optimise
     (Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector)
   is
      From_Index : Positive := 1;
      Known_Values : Known_Value_Maps.Map;

      Result : Tagatha.Transfers.Transfer_Vectors.Vector;
      Label  : Tagatha.Labels.Tagatha_Label := Tagatha.Labels.No_Label;

      function Is_Known (Transfer : Transfer_Operand) return Boolean;
      function Known_Value
        (Transfer : Transfer_Operand)
         return Transfer_Operand
        with Pre => Is_Known (Transfer);

      procedure Record_Value
        (Transfer : Transfer_Operand;
         Value    : Transfer_Operand)
        with Pre => Transfer.Op = T_External;

      procedure Clear_Value
        (Transfer : Transfer_Operand);

      procedure Clear_Known_Registers
        (Changed_Registers : String);

      ---------------------------
      -- Clear_Known_Registers --
      ---------------------------

      procedure Clear_Known_Registers
        (Changed_Registers : String)
      is
         Start : Positive := Changed_Registers'First;
         Index : Positive := Changed_Registers'First;
      begin
         while Index <= Changed_Registers'Last loop
            Index := Index + 1;
            if Index > Changed_Registers'Last
              or else Changed_Registers (Index) = ','
            then
               declare
                  use Ada.Strings.Unbounded;
                  R : constant Unbounded_String :=
                        To_Unbounded_String
                          (Changed_Registers (Start .. Index - 1));
               begin
                  if Known_Values.Contains (R) then
                     --  FIXME: should also delete knowledge of names
                     --  which contain R, since now they do not, and we
                     --  risk ignoring the next transfer of R to that name
                     --  Safest would be to delete everything
                     Known_Values.Delete (R);
                  end if;
                  if Known_Values.Contains (R & "-deref") then
                     Known_Values.Delete (R & "-deref");
                  end if;
               end;
               Start := Index + 1;
            end if;
         end loop;
      end Clear_Known_Registers;

      -----------------
      -- Clear_Value --
      -----------------

      procedure Clear_Value
        (Transfer : Transfer_Operand)
      is
         use Ada.Strings.Unbounded;
      begin
         if Transfer.Op = T_External then
            if not Transfer.Modifiers.Dereferenced
              and then Known_Values.Contains (Transfer.External_Name)
            then
               Known_Values.Delete (Transfer.External_Name);
            end if;

            if Known_Values.Contains (Transfer.External_Name & "-deref") then
               Known_Values.Delete (Transfer.External_Name & "-deref");
            end if;
         end if;
      end Clear_Value;

      --------------
      -- Is_Known --
      --------------

      function Is_Known (Transfer : Transfer_Operand) return Boolean is
         use Ada.Strings.Unbounded;
      begin
         if Transfer.Op = T_External then
            if Transfer.Modifiers.Dereferenced then
               return Known_Values.Contains
                 (Transfer.External_Name & "-deref");
            else
               return Known_Values.Contains
                 (Transfer.External_Name);
            end if;
         else
            return False;
         end if;
      end Is_Known;

      -----------------
      -- Known_Value --
      -----------------

      function Known_Value
        (Transfer : Transfer_Operand)
         return Transfer_Operand
      is
         use Ada.Strings.Unbounded;
      begin
         if Transfer.Modifiers.Dereferenced then
            return Known_Values.Element
              (Transfer.External_Name & "-deref");
         else
            return Known_Values.Element
              (Transfer.External_Name);
         end if;
      end Known_Value;

      ------------------
      -- Record_Value --
      ------------------

      procedure Record_Value
        (Transfer : Transfer_Operand;
         Value    : Transfer_Operand)
      is
         use Ada.Strings.Unbounded;
         Key : constant Unbounded_String :=
                 (if Transfer.Modifiers.Dereferenced
                  then Transfer.External_Name & "-deref"
                  else Transfer.External_Name);
      begin
         if not Transfer.External_Predec
           and then not Transfer.External_Postinc
         then
            if Known_Values.Contains (Key) then
               Known_Values.Replace (Key, Value);
            else
               Known_Values.Insert (Key, Value);
            end if;
         end if;
      end Record_Value;

   begin
      while From_Index <= Transfers.Last_Index loop
         declare
            From : Transfer := Transfers (From_Index);
            Copy : Boolean  := True;
         begin
            if Tagatha.Labels.Has_Label (From.Label)
              or else From.Call
            then
               Known_Values.Clear;
            end if;

            if From_Index < Transfers.Last_Index - 1
              and then Transfers (From_Index).Op = Op_Not
              and then Transfers (From_Index + 1).Op = Op_Test
              and then Transfers (From_Index + 2).Trans = T_Control
              and then Transfers (From_Index + 2).Condition /= C_Always
            then
               From := Transfers (From_Index + 1);
               From.Src_1 := Transfers (From_Index).Src_1;
               if Tagatha.Labels.Has_Label
                 (Transfers.Element (From_Index).Label)
               then
                  From.Label := Transfers.Element (From_Index).Label;
               end if;

               Result.Append (From);

               From := Transfers (From_Index + 2);
               From.Condition := Negate (From.Condition);
               Result.Append (From);

               From_Index := From_Index + 2;
               Copy := False;

            elsif From_Index < Transfers.Last_Index
              and then Transfers (From_Index).Op = Op_Not
              and then Transfers (From_Index + 1).Trans = T_Control
              and then Transfers (From_Index + 1).Condition /= C_Always
            then
               From := Transfers (From_Index + 1);
               if Tagatha.Labels.Has_Label
                 (Transfers.Element (From_Index).Label)
               then
                  From.Label := Transfers.Element (From_Index).Label;
               end if;
               From.Condition := Negate (From.Condition);
               From_Index := From_Index + 1;
            elsif From.Trans = T_Data
              and then From.Op = Op_Nop
              and then Same_Operand (From.Src_1, From.Dst)
            then
               Copy := False;
            elsif From.Trans = T_Data
              and then From.Op = Op_Nop
              and then From.Src_1.Op /= T_Stack
              and then (From.Src_1.Op /= T_External
                        or else From.Src_1.External_Imm)
              and then Is_Known (From.Dst)
              and then Known_Value (From.Dst) = From.Src_1
            then
               Copy := False;
            end if;

            if Copy then
               if Tagatha.Labels.Has_Label (Label) then
                  Tagatha.Labels.Link_To (Label, From.Label);
                  From.Label := Label;
                  Label := Tagatha.Labels.No_Label;
               end if;
               Result.Append (From);

               Clear_Value (From.Dst);

               if From.Trans = T_Data
                 and then From.Op = Op_Nop
                 and then From.Src_1.Op /= T_Stack
                 and then From.Dst.Op = T_External
               then
                  Record_Value (From.Dst, From.Src_1);
               elsif From.Trans = T_Native then
                  declare
                     Rs : constant String :=
                            Ada.Strings.Unbounded.To_String
                              (From.Changed_Registers);
                  begin
                     if Rs /= "" then
                        Clear_Known_Registers (Rs);
                     end if;
                  end;
               end if;
            elsif Tagatha.Labels.Has_Label (From.Label) then
               Tagatha.Labels.Link_To (From.Label, Label);
               Label := From.Label;
               Known_Values.Clear;
            end if;

            From_Index := From_Index + 1;

         end;
      end loop;

      Transfers := Result;

   end Optimise;

end Tagatha.Transfers.Optimiser;
