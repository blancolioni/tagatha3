with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

with Tagatha.Code.Aqua32;
with Tagatha.Code.Pdp11;
with Tagatha.Code.Pdp32;
with Tagatha.Code.I686;
with Tagatha.Code.M6502;
with Tagatha.Code.X86_64;

package body Tagatha.Code is

   function To_Integer is new
     Ada.Unchecked_Conversion (Tagatha_Floating_Point,
                               Floating_Point_Integer);

   ------------------
   -- Address_Size --
   ------------------

   function Address_Size (T : Translator) return Tagatha_Size is
   begin
      return Word_Size (Translator'Class (T));
   end Address_Size;

   ----------
   -- Data --
   ----------

   procedure Data
     (T     : in out Translator;
      Asm   : in out Assembly'Class;
      Value : Tagatha.Constants.Tagatha_Constant)
   is
      pragma Unreferenced (T);
   begin
      Asm.Put_Line ("    .word " & To_String (Value));
   end Data;

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator (Name : String) return Translator'Class is
   begin
      if Name = "pdp-11" then
         return Tagatha.Code.Pdp11.Get_Translator;
      elsif Name = "i686" then
         return Tagatha.Code.I686.Get_Translator;
      elsif Name = "x86_64" then
         return Tagatha.Code.X86_64.Get_Translator;
      elsif Name = "6502" then
         return Tagatha.Code.M6502.Get_Translator;
      elsif Name = "pdp32" then
         return Tagatha.Code.Pdp32.Get_Translator;
      elsif Name = "aqua32" then
         return Tagatha.Code.Aqua32.Get_Translator;
      else
         raise Constraint_Error with
           "unknown target: " & Name;
      end if;
   end Get_Translator;

   -----------
   -- Image --
   -----------

   function Image (Item : Tagatha_Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Tagatha_Integer'Image (Item),
                                     Ada.Strings.Left);
   end Image;

   ------------------
   -- Integer_Size --
   ------------------

   function Integer_Size (T : Translator) return Tagatha_Size is
   begin
      return Word_Size (Translator'Class (T));
   end Integer_Size;

   -------------
   -- Segment --
   -------------

   procedure Segment
     (T     : Translator;
      Asm   : in out Assembly'Class;
      Seg   : Tagatha_Segment)
   is
      pragma Unreferenced (T);
   begin
      case Seg is
         when Executable =>
            Asm.Put_Line (".code");
         when Read_Only =>
            Asm.Put_Line (".text");
         when Read_Write =>
            Asm.Put_Line (".data");
      end case;
   end Segment;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (V    : Tagatha.Constants.Tagatha_Constant;
      From : Tagatha.Transfers.Transfer_Operand :=
        Tagatha.Transfers.No_Operand)
      return String
   is
      use Tagatha.Constants;
      use Tagatha.Transfers;
   begin
      if Is_Integer (V) then
         return Image ((Get_Integer (V) and Get_Slice_Mask (From)) /
                       (2 ** Natural (Get_Slice_Bit_Offset (From))));
      elsif Is_Floating_Point (V) then
         declare
            FPI : constant Floating_Point_Integer :=
                    To_Integer (Get_Floating_Point (V));
            Int : constant Tagatha_Integer :=
                    Tagatha_Integer (FPI);
            Mask : constant Tagatha_Integer := Get_Slice_Mask (From);
            Offset : constant Natural := Natural (Get_Slice_Bit_Offset (From));
         begin
            return Image ((Int and Mask) / 2 ** Offset);
         end;
      elsif Is_Label (V) then
         if Has_Slice (From) then
            if Slice_Fits (From, Size_8) then
               return Tagatha.Labels.Show (Get_Label (V), '_') & " +" &
                 Image (Get_Slice_Octet_Offset (From));
            else
               raise Constraint_Error with
                 "can't take non-Octet slice from label: " &
                 Show (From);
            end if;
         else
            return Tagatha.Labels.Show (Get_Label (V), '_');
         end if;
      else
         raise Constraint_Error with
           "unknown constant type in " & Show (V);
      end if;
   end To_String;

end Tagatha.Code;
