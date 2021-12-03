package Tagatha.Code.I686 is

   type I686_Translator is new Translator with private;

   overriding
   procedure Start (T      : in out I686_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean);

   overriding
   procedure Finish (T   : in out I686_Translator;
                     Asm : in out Assembly'Class);

   overriding
   procedure Encode (T    : in out I686_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer);

   overriding
   procedure Label (T     : in out I686_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label);

   overriding function Get_Register_Range
     (Translator : I686_Translator;
      Data       : Tagatha_Data_Type)
      return Register_Range_Record;

   overriding
   function Word_Size (T : I686_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

private

   type I686_Translator is new Standard_Translator (32) with
      record
         Reverse_Test : Boolean := False;
      end record;

end Tagatha.Code.I686;
