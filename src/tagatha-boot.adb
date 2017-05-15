with Ada.Text_IO;

with Tagatha;                           use Tagatha;
with Tagatha.Units;                     use Tagatha.Units;

package body Tagatha.Boot is
   procedure Boot is

      U_Put : Tagatha_Unit;
      U_Gcd : Tagatha_Unit;

   begin

      Ada.Text_IO.Put_Line ("Put");

      --  procedure Put (Text : String) is
      --     P     : Char_Access;
      --     Count : Positive;
      U_Put.Create_Unit ("_put", "put.adb",
                         Argument_Words => 1,
                         Frame_Words    => 3);

      --   P := Char_Access (Text'Address + 4);
      U_Put.Push_Argument (1);
      U_Put.Push (4);
      U_Put.Operate (Op_Add);
      U_Put.Pop_Local (1);

      --  Count := Positive_Access (Text'Address).all
      U_Put.Push_Argument (1);
      U_Put.Dereference;
      U_Put.Pop_Local (2);

      --  for I in 0 .. Count - 1 loop
      U_Put.Loop_Around ("L1", 2, 3);

      --     Put_Char ((P + I).all);
      U_Put.Push_Local (1);
      U_Put.Push_Local (3);
      U_Put.Operate (Op_Add);
      U_Put.Dereference (Size_8);
      U_Put.Call ("push_char");

      --  end loop
      U_Put.Label (1);

      --  end Put;
      U_Put.Finish_Unit;

      Ada.Text_IO.Put_Line ("Gcd");
      --  function Gcd (X, Y : Positive) return Positive is
      --    T : Positive;
      U_Gcd.Create_Unit ("Gcd", "gcd.c", 2, 1);

      --  begin
      --    while Y /= 0 loop
      U_Gcd.Label (1);
      U_Gcd.Push_Argument (2);
      U_Gcd.Push (0);
      U_Gcd.Operate (Op_Compare);
      U_Gcd.Jump (2, C_Equal);

      --       T := Y;
      U_Gcd.Push_Argument (2);
      U_Gcd.Pop_Local (1);

      --       Y := X mod Y;
      U_Gcd.Push_Argument (1);
      U_Gcd.Push_Argument (2);
      U_Gcd.Operate (Op_Mod);
      U_Gcd.Pop_Argument (2);

      --       X := T;
      U_Gcd.Push_Local (1);
      U_Gcd.Pop_Argument (1);

      --    end loop;
      U_Gcd.Jump (1);
      U_Gcd.Label (2);

      --    return X;
      U_Gcd.Push_Argument (1);
      U_Gcd.Pop_Result;

      --  end Gcd;
      U_Gcd.Finish_Unit;

      Ada.Text_IO.Put_Line ("optimising Put");
      U_Put.Optimise;

      Ada.Text_IO.Put_Line ("optimising Gcd");

      U_Gcd.Optimise;

      U_Put.Write ("x86_64");
      U_Gcd.Write ("x86_64");

   end Boot;

end Tagatha.Boot;
