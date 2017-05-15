package Tagatha.Code.M6502 is

   type M6502_Translator is new Translator with private;

   overriding
   procedure Start (T      : in out M6502_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean);

   overriding
   procedure Finish (T   : in out M6502_Translator;
                     Asm : in out Assembly'Class);

   overriding
   procedure Encode (T    : in out M6502_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer);

   overriding
   procedure Label (T     : in out M6502_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label);

   overriding
   function General_Registers (T : M6502_Translator) return Positive;

   overriding
   function Word_Size (T : M6502_Translator) return Tagatha_Size;

   overriding
   function Address_Size (T : M6502_Translator) return Tagatha_Size;

   overriding
   function Integer_Size (T : M6502_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

private

   type Register_Info is
      record
         Have_Value : Boolean := False;
         Value      : Natural := 0;
      end record;

   type Register is (A, X, Y, FP, SP, A1, A2, A3, A4);

   subtype Index_Register is Register range X .. Y;
   subtype Address_Register is Register range A1 .. A4;

   type Register_Info_Array is array (Register) of Register_Info;

   type M6502_Translator is new Translator with
      record
         Reverse_Test  : Boolean := False;
         MSB_First     : Boolean := False;
         Registers     : Register_Info_Array;
         Y_Decremented : Boolean := False;
         Y_Is_FP       : Boolean := False;
         X_Is_FP       : Boolean := False;
         Frame_Size    : Natural := 0;
         Source_Addr   : Address_Register := A1;
         Dest_Addr     : Address_Register := A2;
      end record;

end Tagatha.Code.M6502;
