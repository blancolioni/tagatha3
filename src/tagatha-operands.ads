private with Ada.Strings.Unbounded;

with Tagatha.Constants;
with Tagatha.Labels;

package Tagatha.Operands is

   type Tagatha_Operand is private;

   function Null_Operand return Tagatha_Operand;
   function Unknown_Operand return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha.Constants.Tagatha_Constant)
                              return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha_Floating_Point)
                              return Tagatha_Operand;

   function Constant_Operand (Value : Tagatha_Integer)
                              return Tagatha_Operand;

   function Label_Operand (Label : Tagatha.Labels.Tagatha_Label)
                          return Tagatha_Operand;

   function Argument_Operand (Offset : Argument_Offset)
                              return Tagatha_Operand;

   function Local_Operand (Offset : Local_Offset)
                           return Tagatha_Operand;

   function Result_Operand return Tagatha_Operand;

   function External_Operand (Name : String;
                              Immediate : Boolean;
                              Volatile  : Boolean := False)
                              return Tagatha_Operand;

   function Register_Operand
     (Name        : String;
      Dereference : Boolean := False)
      return Tagatha_Operand;

   function Text_Operand (Text : String)
                          return Tagatha_Operand;

   function Is_Constant (Item : Tagatha_Operand) return Boolean;
   function Is_Argument (Item : Tagatha_Operand) return Boolean;
   function Is_Local    (Item : Tagatha_Operand) return Boolean;
   function Is_Result   (Item : Tagatha_Operand) return Boolean;
   function Is_External (Item : Tagatha_Operand) return Boolean;
   function Is_Text (Item : Tagatha_Operand) return Boolean;
   function Is_Immediate (Item : Tagatha_Operand) return Boolean;
   function Is_Unknown (Item : Tagatha_Operand) return Boolean;
   function Is_Dereferenced (Item : Tagatha_Operand) return Boolean;

   function Get_Value (Item : Tagatha_Operand)
                      return Tagatha.Constants.Tagatha_Constant;
   function Get_Arg_Offset (Item : Tagatha_Operand) return Argument_Offset;
   function Get_Local_Offset (Item : Tagatha_Operand) return Local_Offset;

   function Get_Name (Item : Tagatha_Operand) return String;
   function Get_Text (Item : Tagatha_Operand) return String;

   function Show (Operand : Tagatha_Operand) return String;

private

   type Tagatha_Operand_Type is
     (O_Unknown,
      O_Constant,
      O_Argument,
      O_Local,
      O_Result,
      O_External,
      O_Text);

   type Tagatha_Operand_Record (Operand_Type : Tagatha_Operand_Type) is
      record
         Dereference : Boolean;
         case Operand_Type is
            when O_Unknown =>
               null;
            when O_Constant =>
               Value         : Tagatha.Constants.Tagatha_Constant;
            when O_Argument =>
               Arg_Offset    : Argument_Offset;
            when O_Local =>
               Loc_Offset    : Local_Offset;
            when O_Result =>
               null;
            when O_External =>
               Ext_Label     : Ada.Strings.Unbounded.Unbounded_String;
               Ext_Immediate : Boolean;
               Ext_Register  : Boolean;
               Ext_Volatile  : Boolean;
            when O_Text =>
               Text          : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   type Tagatha_Operand is access Tagatha_Operand_Record;

   function Is_Dereferenced (Item : Tagatha_Operand) return Boolean
   is (Item.Dereference);

end Tagatha.Operands;
