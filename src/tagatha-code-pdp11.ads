package Tagatha.Code.Pdp11 is

   type Pdp11_Translator is new Translator with private;

   overriding
   procedure Start (T      : in out Pdp11_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean);

   overriding
   procedure Finish (T   : in out Pdp11_Translator;
                     Asm : in out Assembly'Class);

   overriding
   procedure Encode (T    : in out Pdp11_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer);

   overriding
   procedure Label (T     : in out Pdp11_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label);

   overriding function Get_Register_Range
     (Translator : Pdp11_Translator;
      Data       : Tagatha_Data_Type)
      return Register_Range_Record;

   overriding
   function Word_Size (T : Pdp11_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

private

   type Pdp11_Translator is new Standard_Translator (16) with
      record
         Operator     : Tagatha_Operator := Op_Nop;
         Reverse_Test : Boolean := False;
      end record;

end Tagatha.Code.Pdp11;
