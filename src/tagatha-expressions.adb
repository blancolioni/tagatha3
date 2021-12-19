package body Tagatha.Expressions is

   -------------------
   -- Get_Transfers --
   -------------------

   function Get_Transfers (Temps : Tagatha.Temporaries.Temporary_Source;
                           Expr  : Expression;
                           Dst   : Tagatha.Transfers.Transfer_Operand)
                          return Tagatha.Transfers.Array_Of_Transfers
   is
      use Tagatha.Transfers;

      function Expression_Transfers (E      : Expression;
                                     Dst    : Transfer_Operand)
                                    return Array_Of_Transfers;

      function Subexpression_Transfers (Sub : Expression;
                                        Op  : Transfer_Operand)
                                       return Array_Of_Transfers;

      function Subexpression_Transfer_Op (Sub : Expression)
                                         return Transfer_Operand;

      --------------------------
      -- Expression_Transfers --
      --------------------------

      function Expression_Transfers (E      : Expression;
                                     Dst    : Transfer_Operand)
                                    return Array_Of_Transfers
      is
      begin
         case E.Class is
            when Simple =>
               if Expr.Term = Dst then
                  return No_Transfers;
               else
                  return (1 => Simple_Transfer (Expr.Term, Dst));
               end if;

            when Structured =>
               if E.Op in Zero_Argument_Operator then
                  raise Constraint_Error with
                    "attempt to get transfers for a zero argument operator";
               elsif E.Op = Op_Dereference then
                  declare
                     Left_Op     : constant Transfer_Operand :=
                                     Subexpression_Transfer_Op (E.Left);
                     Left_Exprs  : constant Array_Of_Transfers :=
                                     Subexpression_Transfers (E.Left, Left_Op);
                  begin
                     return Left_Exprs &
                       Operation_Transfer (Left_Op, E.Right.Term,
                                           E.Op, Dst);
                  end;

               elsif E.Op in One_Argument_Operator then
                  declare
                     Left_Op     : constant Transfer_Operand :=
                       Subexpression_Transfer_Op (E.Left);
                     Left_Exprs  : constant Array_Of_Transfers :=
                       Subexpression_Transfers (E.Left, Left_Op);
                  begin
                     return Left_Exprs &
                       Operation_Transfer (Left_Op, No_Operand,
                                           E.Op, Dst);
                  end;
               else
                  declare
                     Left_Op     : constant Transfer_Operand :=
                       Subexpression_Transfer_Op (E.Left);
                     Right_Op    : constant Transfer_Operand :=
                       Subexpression_Transfer_Op (E.Right);
                     Left_Exprs  : constant Array_Of_Transfers :=
                       Subexpression_Transfers (E.Left, Left_Op);
                     Right_Exprs : constant Array_Of_Transfers :=
                       Subexpression_Transfers (E.Right, Right_Op);
                  begin
                     return Left_Exprs & Right_Exprs &
                       Operation_Transfer (Left_Op, Right_Op, E.Op, Dst);
                  end;
               end if;
         end case;
      end Expression_Transfers;

      -------------------------------
      -- Subexpression_Transfer_Op --
      -------------------------------

      function Subexpression_Transfer_Op
        (Sub : Expression)
        return Transfer_Operand
      is
      begin
         case Sub.Class is
            when Simple =>
               return Sub.Term;
            when Structured =>
               declare
                  T : constant Tagatha.Temporaries.Temporary :=
                    Tagatha.Temporaries.Next_Temporary (Temps);
               begin
                  if Sub.Op = Op_Dereference then
                     return Temporary_Operand (T, Sub.Right.Term);
                  else
                     return Temporary_Operand (T);
                  end if;
               end;
         end case;
      end Subexpression_Transfer_Op;

      -----------------------------
      -- Subexpression_Transfers --
      -----------------------------

      function Subexpression_Transfers
        (Sub : Expression;
         Op  : Transfer_Operand)
        return Array_Of_Transfers
      is
      begin
         case Sub.Class is
            when Simple =>
               declare
                  Result : Array_Of_Transfers (1 .. 0);
               begin
                  return Result;
               end;
            when Structured =>
               return Expression_Transfers (Sub, Op);
         end case;
      end Subexpression_Transfers;

   begin
      return Expression_Transfers (Expr, Dst);
   end Get_Transfers;

   -------------------
   -- Get_Transfers --
   -------------------

   function Get_Transfers
     (Temps   : Tagatha.Temporaries.Temporary_Source;
      Address : Expression;
      Value   : Expression)
      return Tagatha.Transfers.Array_Of_Transfers
   is
      use Tagatha.Transfers;
      T_Address : constant Transfers.Transfer_Operand :=
                    Transfers.Temporary_Operand
                      (Tagatha.Temporaries.Next_Temporary (Temps));
      T_Value   : constant Transfers.Transfer_Operand :=
                    Transfers.Temporary_Operand
                      (Tagatha.Temporaries.Next_Temporary (Temps));
      Addr_Ts   : constant Tagatha.Transfers.Array_Of_Transfers :=
                    Get_Transfers (Temps, Address, T_Address);
      Value_Ts  : constant Tagatha.Transfers.Array_Of_Transfers :=
                    Get_Transfers (Temps, Value, T_Value);
   begin
      return Addr_Ts
        & Value_Ts
        & Tagatha.Transfers.Simple_Transfer
        (T_Value, T_Address, To_Address => True);
   end Get_Transfers;

   -----------
   -- Image --
   -----------

   function Image (Expr : Expression) return String is
   begin
      case Expr.Class is
         when Simple =>
            return Tagatha.Transfers.Show (Expr.Term);
         when Structured =>
            if Expr.Right = null then
               return Tagatha_Operator'Image (Expr.Op)
                 & " "
                 & (if Expr.Left.Class = Simple
                    then Image (Expr.Left)
                    else "(" & Image (Expr.Left) & ")");
            else
               return Tagatha_Operator'Image (Expr.Op)
                 & " "
                 & (if Expr.Left.Class = Simple
                    then Image (Expr.Left)
                    else "(" & Image (Expr.Left) & ")")
                 & " "
                 & (if Expr.Right.Class = Simple
                    then Image (Expr.Right)
                    else "(" & Image (Expr.Right) & ")");
            end if;
      end case;
   end Image;

   --------------------------------
   -- New_Dereference_Expression --
   --------------------------------

   function New_Dereference_Expression
     (Arg  : Expression;
      Data : Tagatha_Data_Type;
      Size : Tagatha_Size)
      return Expression
   is
   begin
      return new Expression_Record'
        (Class => Structured,
         Op    => Op_Dereference,
         Left  => Arg,
         Right =>
           New_Simple_Expression
             (Tagatha.Transfers.Type_Operand (Data, Size)));
   end New_Dereference_Expression;

   -----------------------------
   -- New_Operator_Expression --
   -----------------------------

   function New_Operator_Expression
     (Op   : Zero_Argument_Operator)
     return Expression
   is
   begin
      return new Expression_Record'(Structured, Op, null, null);
   end New_Operator_Expression;

   -----------------------------
   -- New_Operator_Expression --
   -----------------------------

   function New_Operator_Expression
     (Op   : One_Argument_Operator;
      Arg  : Expression)
     return Expression
   is
   begin
      return new Expression_Record'(Structured, Op, Arg, null);
   end New_Operator_Expression;

   -----------------------------
   -- New_Operator_Expression --
   -----------------------------

   function New_Operator_Expression
     (Op          : Two_Argument_Operator;
      Left, Right : Expression)
     return Expression
   is
   begin
      return new Expression_Record'(Structured, Op, Left, Right);
   end New_Operator_Expression;

   ---------------------------
   -- New_Simple_Expression --
   ---------------------------

   function New_Simple_Expression (Term : Tagatha.Transfers.Transfer_Operand)
                                  return Expression
   is
   begin
      return new Expression_Record'(Simple, Term);
   end New_Simple_Expression;

end Tagatha.Expressions;
