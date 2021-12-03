with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Transfers;

package Tagatha.Code is

   type Assembly is limited interface;

   procedure Put_Line (A    : in out Assembly;
                       Line : in     String)
   is abstract;

   type Translator is abstract tagged private;

   function Address_Bits (T : Translator) return Natural is abstract;
   function Default_Size_Bits (T : Translator) return Natural is abstract;

   function Default_Integer_Bits (T : Translator) return Natural
   is (Translator'Class (T).Default_Size_Bits);

   function Default_Floating_Point_Bits (T : Translator) return Natural
   is (32);

   function Size_Bits
     (T    : Translator'Class;
      Size : Tagatha_Size)
      return Natural;

   procedure File_Preamble (T                : in out Translator;
                            Asm              : in out Assembly'Class;
                            Source_File_Name : in     String)
   is null;

   procedure File_Postamble (T                : in out Translator;
                             Asm              : in out Assembly'Class)
   is null;

   procedure Set_Location
     (T      : in out Translator;
      Asm    : in out Assembly'Class;
      Line   : Positive;
      Column : Positive)
   is null;

   procedure Start (T      : in out Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean)
      is abstract;

   procedure Finish (T : in out Translator;
                     Asm  : in out Assembly'Class)
      is abstract;

   procedure Begin_Frame (T           : in out Translator;
                          Asm  : in out Assembly'Class;
                          Return_Count    : Natural;
                          Arg_Count       : Natural;
                          Local_Count     : Natural;
                          Temporary_Count : Natural)
   is null;

   procedure End_Frame (T           : in out Translator;
                        Asm         : in out Assembly'Class;
                        Arg_Count   : in     Natural;
                        Local_Count : in     Natural)
   is null;

   procedure Encode (T    : in out Translator;
                     Asm  : in out Assembly'Class;
                     Item : Tagatha.Transfers.Transfer)
      is abstract;

   procedure Label (T     : in out Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label)
     is abstract;

   type Register_Range_Record is
      record
         First : Positive;
         Last  : Natural;
      end record;

   function Get_Register_Range
     (T    : Translator;
      Data : Tagatha_Data_Type)
      return Register_Range_Record
      is abstract;

   procedure Segment
     (T     : Translator;
      Asm   : in out Assembly'Class;
      Seg   : Tagatha_Segment);

   function Extension
     (T : Translator)
      return String
   is (".s");

   procedure Data
     (T     : in out Translator;
      Asm   : in out Assembly'Class;
      Value : Tagatha.Constants.Tagatha_Constant);

   function Word_Size (T : Translator) return Tagatha_Size is abstract;
   function Address_Size (T : Translator) return Tagatha_Size;
   function Integer_Size (T : Translator) return Tagatha_Size;

   function Get_Translator (Name : String) return Translator'Class;

   type Standard_Translator (Bits : Natural) is
     abstract new Translator with private;

   overriding function Address_Bits (T : Standard_Translator) return Natural
   is (T.Bits);

   overriding function Default_Size_Bits
     (T : Standard_Translator)
      return Natural
   is (T.Bits);

private

   type Translator is abstract tagged null record;

   function Image (Item : Tagatha_Integer) return String;

   function To_String
     (V : Tagatha.Constants.Tagatha_Constant;
      From : Tagatha.Transfers.Transfer_Operand :=
        Tagatha.Transfers.No_Operand)
      return String;

   type Standard_Translator (Bits : Natural) is
     abstract new Translator with null record;

end Tagatha.Code;
