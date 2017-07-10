package body Tagatha.Operands is

   Local_Unknown_Operand : Tagatha_Operand;

   ----------------------
   -- Argument_Operand --
   ----------------------

   function Argument_Operand (Offset : Argument_Offset)
                             return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Argument, False, Offset);
   end Argument_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                             return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Constant, False, Value);
   end Constant_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha_Floating_Point)
                             return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Floating_Point_Constant (Value));
   end Constant_Operand;

   ----------------------
   -- Constant_Operand --
   ----------------------

   function Constant_Operand (Value : Tagatha_Integer)
                             return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Integer_Constant (Value));
   end Constant_Operand;

   ----------------------
   -- External_Operand --
   ----------------------

   function External_Operand (Name : String;
                              Immediate : Boolean;
                              Volatile  : Boolean := False)
                              return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'
        (O_External, False, Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Ext_Immediate => Immediate,
         Ext_Register  => False,
         Ext_Volatile  => Volatile);
   end External_Operand;

   --------------------
   -- Get_Arg_Offset --
   --------------------

   function Get_Arg_Offset (Item : Tagatha_Operand) return Argument_Offset is
   begin
      return Item.Arg_Offset;
   end Get_Arg_Offset;

   ----------------------
   -- Get_Local_Offset --
   ----------------------

   function Get_Local_Offset (Item : Tagatha_Operand) return Local_Offset is
   begin
      return Item.Loc_Offset;
   end Get_Local_Offset;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : Tagatha_Operand) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Ext_Label);
   end Get_Name;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Item : Tagatha_Operand) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Text);
   end Get_Text;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Tagatha_Operand)
                      return Tagatha.Constants.Tagatha_Constant
   is
   begin
      return Item.Value;
   end Get_Value;

   -----------------
   -- Is_Argument --
   -----------------

   function Is_Argument (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Argument;
   end Is_Argument;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Constant;
   end Is_Constant;

   -----------------
   -- Is_External --
   -----------------

   function Is_External (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_External;
   end Is_External;

   ------------------
   -- Is_Immediate --
   ------------------

   function Is_Immediate (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Ext_Immediate;
   end Is_Immediate;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Local;
   end Is_Local;

   ---------------
   -- Is_Result --
   ---------------

   function Is_Result (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Result;
   end Is_Result;

   -------------
   -- Is_Text --
   -------------

   function Is_Text (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Text;
   end Is_Text;

   ----------------
   -- Is_Unknown --
   ----------------

   function Is_Unknown (Item : Tagatha_Operand) return Boolean is
   begin
      return Item.Operand_Type = O_Unknown;
   end Is_Unknown;

   -------------------
   -- Label_Operand --
   -------------------

   function Label_Operand (Label : Tagatha.Labels.Tagatha_Label)
                          return Tagatha_Operand
   is
   begin
      return Constant_Operand (Constants.Label_Constant (Label));
   end Label_Operand;

   --------=----------
   -- Local_Operand --
   -------------------

   function Local_Operand (Offset : Local_Offset)
                          return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'(O_Local, False, Offset);
   end Local_Operand;

   ------------------
   -- Null_Operand --
   ------------------

   function Null_Operand return Tagatha_Operand is
   begin
      return null;
   end Null_Operand;

   ----------------------
   -- Register_Operand --
   ----------------------

   function Register_Operand
     (Name        : String;
      Dereference : Boolean := False)
      return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'
        (Operand_Type  => O_External,
         Dereference   => Dereference,
         Ext_Label     => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Ext_Immediate => False,
         Ext_Register  => True,
         Ext_Volatile  => False);
   end Register_Operand;

   --------------------
   -- Result_Operand --
   --------------------

   function Result_Operand return Tagatha_Operand is
   begin
      return new Tagatha_Operand_Record'
        (Operand_Type => O_Result, Dereference => False);
   end Result_Operand;

   ----------
   -- Show --
   ----------

   function Show (Operand : Tagatha_Operand) return String is
   begin
      if Operand = null then
         return "<>";
      end if;
      case Operand.Operand_Type is
         when O_Constant =>
            return Constants.Show (Operand.Value);
         when O_Argument =>
            return "arg" & Argument_Offset'Image (-Operand.Arg_Offset);
         when O_External =>
            if Operand.Ext_Immediate then
               return "#"
                 & Ada.Strings.Unbounded.To_String (Operand.Ext_Label);
            else
               return Ada.Strings.Unbounded.To_String (Operand.Ext_Label);
            end if;
         when O_Local =>
            return "loc" & Local_Offset'Image (-Operand.Loc_Offset);
         when O_Result =>
            return "result";
         when O_Text =>
            return """" & Ada.Strings.Unbounded.To_String (Operand.Text)
              & """";
         when O_Unknown =>
            return "?";
      end case;
   end Show;

   ------------------
   -- Text_Operand --
   ------------------

   function Text_Operand (Text : String)
                          return Tagatha_Operand
   is
   begin
      return new Tagatha_Operand_Record'
        (O_Text, False, Ada.Strings.Unbounded.To_Unbounded_String (Text));
   end Text_Operand;

   ---------------------
   -- Unknown_Operand --
   ---------------------

   function Unknown_Operand return Tagatha_Operand is
   begin
      if Local_Unknown_Operand = null then
         Local_Unknown_Operand :=
           new Tagatha_Operand_Record'
             (Operand_Type => O_Unknown, Dereference => False);
      end if;
      return Local_Unknown_Operand;
   end Unknown_Operand;

end Tagatha.Operands;
