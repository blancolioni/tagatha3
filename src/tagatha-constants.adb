package body Tagatha.Constants is

   ----------------------
   -- Address_Constant --
   ----------------------

   function Address_Constant
     (Value : Tagatha_Integer)
      return Tagatha_Constant
   is
   begin
      return (V_Address, Value);
   end Address_Constant;

   -----------------------------
   -- Floating_Point_Constant --
   -----------------------------

   function Floating_Point_Constant
     (Value : Tagatha_Floating_Point)
      return Tagatha_Constant
   is
   begin
      return (V_Floating_Point, Value);
   end Floating_Point_Constant;

   ------------------------
   -- Get_Floating_Point --
   ------------------------

   function Get_Floating_Point (Item : Tagatha_Constant)
                               return Tagatha_Floating_Point is
   begin
      return Item.Floating_Point_Value;
   end Get_Floating_Point;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Item : Tagatha_Constant) return Tagatha_Integer is
   begin
      return Item.Integer_Value;
   end Get_Integer;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Item : Tagatha_Constant)
                      return Tagatha.Labels.Tagatha_Label
   is
   begin
      return Item.Label_Value;
   end Get_Label;

   ---------------
   -- Get_Octet --
   ---------------

   function Get_Octet (From_Constant : Tagatha_Constant;
                       Octet_Index   : Positive)
                       return Natural
   is
   begin
      case From_Constant.Value_Type is
         when V_Integer | V_Address =>
            return Natural (From_Constant.Integer_Value
                            / (2 ** ((Octet_Index - 1) * 8))
                            mod 256);
         when V_Floating_Point =>
            return 0;
         when V_Label =>
            return 0;
      end case;
   end Get_Octet;

   ----------------------
   -- Integer_Constant --
   ----------------------

   function Integer_Constant
     (Value : Tagatha_Integer)
      return Tagatha_Constant
   is
   begin
      return (V_Integer, Value);
   end Integer_Constant;

   -----------------------
   -- Is_Floating_Point --
   -----------------------

   function Is_Floating_Point (Item : Tagatha_Constant) return Boolean is
   begin
      return Item.Value_Type = V_Floating_Point;
   end Is_Floating_Point;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer        (Item : Tagatha_Constant) return Boolean is
   begin
      return Item.Value_Type = V_Integer;
   end Is_Integer;

   --------------
   -- Is_Label --
   --------------

   function Is_Label          (Item : Tagatha_Constant) return Boolean is
   begin
      return Item.Value_Type = V_Label;
   end Is_Label;

   --------------------
   -- Label_Constant --
   --------------------

   function Label_Constant
     (Value : Tagatha.Labels.Tagatha_Label)
      return Tagatha_Constant
   is
   begin
      return (V_Label, Value);
   end Label_Constant;

   ----------
   -- Show --
   ----------

   function Show (Item : Tagatha_Constant) return String is
   begin
      case Item.Value_Type is
         when V_Integer =>
            return Item.Integer_Value'Image;
         when V_Address =>
            return Item.Address_Value'Image;
         when V_Floating_Point =>
            return Item.Floating_Point_Value'Image;
         when V_Label =>
            return Tagatha.Labels.Show (Item.Label_Value, 'L');
      end case;
   end Show;

end Tagatha.Constants;
