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

   overriding
   function General_Registers (T : Pdp11_Translator) return Positive;

   overriding
   function Word_Size (T : Pdp11_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

private

   type Pdp11_Translator is new Standard_Translator (16) with
      record
         Reverse_Test : Boolean := False;
      end record;

end Tagatha.Code.Pdp11;
