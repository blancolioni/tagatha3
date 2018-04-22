with Tagatha.Temporaries;
with Tagatha.Transfers;

package Tagatha.Expressions is

   type Expression is private;

   function New_Simple_Expression (Term : Tagatha.Transfers.Transfer_Operand)
                                  return Expression;

   function Image (Expr : Expression) return String;

   function New_Operator_Expression
     (Op   : Zero_Argument_Operator)
     return Expression;

   function New_Operator_Expression
     (Op    : One_Argument_Operator;
      Arg   : Expression)
     return Expression;

   function New_Operator_Expression
     (Op          : Two_Argument_Operator;
      Left, Right : Expression)
     return Expression;

   function Get_Transfers
     (Temps   : Tagatha.Temporaries.Temporary_Source;
      Address : Expression;
      Value   : Expression)
      return Tagatha.Transfers.Array_Of_Transfers;
   --  address is destination to store value

   function Get_Transfers (Temps : Tagatha.Temporaries.Temporary_Source;
                           Expr  : Expression;
                           Dst   : Tagatha.Transfers.Transfer_Operand)
                          return Tagatha.Transfers.Array_Of_Transfers;

private

   type Expression_Class is (Simple, Structured);

   type Expression_Record (Class : Expression_Class) is
      record
         case Class is
            when Simple =>
               Term        : Tagatha.Transfers.Transfer_Operand;
            when Structured =>
               Op          : Tagatha_Operator;
               Left, Right : Expression;
         end case;
      end record;

   type Expression is access Expression_Record;

end Tagatha.Expressions;
