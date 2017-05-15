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
                  return Temporary_Operand (T);
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
