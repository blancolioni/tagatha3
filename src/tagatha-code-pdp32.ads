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

   overriding procedure Begin_Frame
     (T               : in out Pdp32_Translator;
      Asm             : in out Assembly'Class;
      Return_Count    : Natural;
      Arg_Count       : Natural;
      Local_Count     : Natural;
      Temporary_Count : Natural);

   overriding procedure End_Frame
     (T           : in out Pdp32_Translator;
      Asm         : in out Assembly'Class;
      Arg_Count   : in     Natural;
      Local_Count : in     Natural);

   overriding procedure Encode
     (Translator : in out Pdp32_Translator;
      Asm        : in out Assembly'Class;
      Item       : in     Tagatha.Transfers.Transfer);

   overriding procedure Label
     (Translator : in out Pdp32_Translator;
      Asm        : in out Assembly'Class;
      Label      : in     Tagatha.Labels.Tagatha_Label);

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
         Ret_Count    : Natural := 0;
         Arg_Count    : Natural := 0;
         Local_Count  : Natural := 0;
         Temp_Count   : Natural := 0;
         Stack_Count  : access Natural;
      end record;

   function To_String
     (Translator : Pdp32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand;
      Source     : Boolean)
      return String;

   function To_Dereferenced_String
     (Translator : Pdp32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand)
      return String;

   function To_Src
     (Translator : Pdp32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand)
      return String
   is (Translator.To_String (Item, True));

   function To_Dst
     (Translator : Pdp32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand)
      return String
   is (Translator.To_String (Item, False));

end Tagatha.Code.Pdp32;
