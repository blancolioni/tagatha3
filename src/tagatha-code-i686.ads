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

   overriding
   function General_Registers (T : I686_Translator) return Positive;

   overriding
   function Word_Size (T : I686_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

private

   type I686_Translator is new Translator with
      record
         Reverse_Test : Boolean := False;
      end record;

end Tagatha.Code.I686;
