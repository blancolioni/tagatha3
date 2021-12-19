package body Tagatha.Commands.Registry is

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Register  : in out Tagatha.Registry.Tagatha_Registry;
      Command   : in     Tagatha_Command)
   is
   begin
      Register.Record_Label (Get_Label (Command));

      if Command.Line > 0 then
         Register.Record_Location (Command.Line, Command.Column);
      end if;

      case Command.Instruction is
         when T_Stack =>
            case Command.Stack_Op is
               when S_Push =>
                  Register.Record_Push
                    (Command.Operand);
               when S_Pop =>
                  Register.Record_Pop (Command.Operand);
               when S_Drop =>
                  Register.Record_Drop (Transfers.Get_Size (Command.Operand));
               when S_Duplicate =>
                  Register.Record_Duplicate
                    (Transfers.Get_Size (Command.Operand));
               when S_Swap =>
                  Register.Record_Swap
                    (Transfers.Get_Size (Command.Operand));
               when S_Store =>
                  Register.Record_Store
                    (Transfers.Get_Size (Command.Operand));
            end case;
         when T_Operate =>
            Register.Record_Operation (Command.Operator);
         when T_Dereference =>
            Register.Record_Dereference (Command.Data, Command.Size);
         when T_Call =>
            Register.Record_Call (Command.Subroutine, Command.Arguments);
         when T_Loop =>
            Register.Record_Loop (Command.Limit,
                                  Command.Counter, Command.End_Label);
         when T_Jump =>
            Register.Record_Jump (Command.Condition,
                                  Command.Destination);
         when T_Native =>
            Register.Record_Native_Operation
              (Ada.Strings.Unbounded.To_String (Command.Native_Name),
               Ada.Strings.Unbounded.To_String (Command.Changed_Registers),
               Command.Input_Words, Command.Output_Words);
      end case;
   end Register_Command;

end Tagatha.Commands.Registry;
