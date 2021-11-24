with Tagatha.Labels;

package Tagatha.Constants is

   type Tagatha_Constant is private;

   function Integer_Constant
     (Value : Tagatha_Integer)
      return Tagatha_Constant;

   function Address_Constant
     (Value : Tagatha_Integer)
      return Tagatha_Constant;

   function Floating_Point_Constant
     (Value : Tagatha_Floating_Point)
      return Tagatha_Constant;

   function Label_Constant
     (Value : Tagatha.Labels.Tagatha_Label)
      return Tagatha_Constant;

   function Is_Integer        (Item : Tagatha_Constant) return Boolean;
   function Is_Floating_Point (Item : Tagatha_Constant) return Boolean;
   function Is_Label          (Item : Tagatha_Constant) return Boolean;

   function Get_Integer (Item : Tagatha_Constant)
                        return Tagatha_Integer;
   function Get_Floating_Point (Item : Tagatha_Constant)
                               return Tagatha_Floating_Point;
   function Get_Label (Item : Tagatha_Constant)
                      return Tagatha.Labels.Tagatha_Label;
   function Show (Item : Tagatha_Constant) return String;

   function Get_Octet (From_Constant : Tagatha_Constant;
                       Octet_Index   : Positive)
                       return Natural;

private

   type Constant_Value_Type is (V_Integer,
                                V_Floating_Point,
                                V_Address,
                                V_Label);

   type Tagatha_Constant
     (Value_Type : Constant_Value_Type := V_Integer) is
      record
         case Value_Type is
            when V_Integer =>
               Integer_Value : Tagatha_Integer;
            when V_Floating_Point =>
               Floating_Point_Value : Tagatha_Floating_Point;
            when V_Address =>
               Address_Value : Tagatha_Integer;
            when V_Label =>
               Label_Value : Tagatha.Labels.Tagatha_Label;
         end case;
      end record;

end Tagatha.Constants;
