package Tagatha.Code.Pdp32 is

   type Pdp32_Translator is new Translator with private;

   overriding procedure File_Preamble
     (T                : in out Pdp32_Translator;
      Asm              : in out Assembly'Class;
      Source_File_Name : in     String);

   overriding procedure Set_Location
     (T      : in out Pdp32_Translator;
      Asm    : in out Assembly'Class;
      Line   : Positive;
      Column : Positive);

   overriding
   procedure Start (T      : in out Pdp32_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean);

   overriding
   procedure Finish (T   : in out Pdp32_Translator;
                     Asm : in out Assembly'Class);

   overriding
   procedure Encode (T    : in out Pdp32_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer);

   overriding
   procedure Label (T     : in out Pdp32_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label);

   overriding
   function General_Registers (T : Pdp32_Translator) return Positive;

   overriding
   function Word_Size (T : Pdp32_Translator) return Tagatha_Size;

   overriding function Extension
     (T : Pdp32_Translator)
      return String
   is (".m32");

   function Get_Translator return Translator'Class;

private

   type Pdp32_Translator is new Translator with
      record
         Reverse_Test : Boolean := False;
      end record;

end Tagatha.Code.Pdp32;
