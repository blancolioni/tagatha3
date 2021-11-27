package Tagatha.Code.Aqua32 is

   type Aqua32_Translator is new Translator with private;

   overriding procedure File_Preamble
     (T                : in out Aqua32_Translator;
      Asm              : in out Assembly'Class;
      Source_File_Name : in     String);

   overriding procedure Set_Location
     (T      : in out Aqua32_Translator;
      Asm    : in out Assembly'Class;
      Line   : Positive;
      Column : Positive);

   overriding procedure Start
     (T         : in out Aqua32_Translator;
      Asm       : in out Assembly'Class;
      Name      : in     String;
      Global    : in     Boolean);

   overriding
   procedure Finish (T   : in out Aqua32_Translator;
                     Asm : in out Assembly'Class);

   overriding procedure Begin_Frame
     (T               : in out Aqua32_Translator;
      Asm             : in out Assembly'Class;
      Return_Count    : Natural;
      Arg_Count       : Natural;
      Local_Count     : Natural;
      Temporary_Count : Natural);

   overriding procedure End_Frame
     (T           : in out Aqua32_Translator;
      Asm         : in out Assembly'Class;
      Arg_Count   : in     Natural;
      Local_Count : in     Natural);

   overriding
   procedure Encode (Translator : in out Aqua32_Translator;
                     Asm        : in out Assembly'Class;
                     Item       : in     Tagatha.Transfers.Transfer);

   overriding procedure Label
     (Translator : in out Aqua32_Translator;
      Asm        : in out Assembly'Class;
      Label      : in     Tagatha.Labels.Tagatha_Label);

   overriding
   function General_Registers (T : Aqua32_Translator) return Positive;

   overriding
   function Word_Size (T : Aqua32_Translator) return Tagatha_Size;

   overriding function Extension
     (T : Aqua32_Translator)
      return String
   is (".aqx");

   function Get_Translator return Translator'Class;

private

   type Register_Category is
     (Argument, Result, Local, Temporary, Scratch, Utility, Stack);

   type Category_Record is
      record
         First : Natural := 0;
         Count : Natural := 0;
      end record;

   type Category_Array is array (Register_Category) of Category_Record;

   type Aqua32_Translator is new Standard_Translator (32) with
      record
         Reverse_Test   : Boolean := False;
         Has_Frame      : Boolean := False;
         Preserve       : Natural := 0;
         Registers      : Category_Array;
      end record;

   procedure Before_Operand
     (Translator : in out Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Operand    : in     Tagatha.Transfers.Transfer_Operand);

   procedure After_Operand
     (Translator : in out Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Operand    : in     Tagatha.Transfers.Transfer_Operand);

   procedure Instruction
     (Translator : in out Aqua32_Translator'Class;
      Asm        : in out Assembly'Class;
      Mnemonic   : in     String;
      Dest       : in     Tagatha.Transfers.Transfer_Operand;
      Source_1   : in     Tagatha.Transfers.Transfer_Operand;
      Source_2   : in     Tagatha.Transfers.Transfer_Operand);

   function To_String
     (Translator : in out Aqua32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand;
      Source     : Boolean)
      return String;

   function Register_Image
     (Translator : Aqua32_Translator'Class;
      Register   : Natural)
      return String;

   function Category_Register
     (Translator : Aqua32_Translator'Class;
      Category   : Register_Category;
      Offset     : Natural)
      return Natural
   is (Translator.Registers (Category).First + Offset)
   with Pre => Offset < Translator.Registers (Category).Count;

   function Argument_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Argument_Offset)
      return Natural
   is (Translator.Category_Register
       (Argument, Natural (Offset - Argument_Offset'First)));

   function Local_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Local_Offset)
      return Natural
   is (Translator.Category_Register
       (Local, Natural (Offset) - 1));

   function Result_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Positive)
      return Natural
   is (Translator.Category_Register
       (Result, Natural (Offset) - 1));

   function Stack_Register
     (Translator : Aqua32_Translator'Class)
      return Natural
   is (Translator.Registers (Stack).First
       + Translator.Registers (Stack).Count - 1);

   function Return_Value_Register
     (Translator : Aqua32_Translator'Class)
      return Natural
   is (Translator.Registers (Stack).First
       + Translator.Registers (Stack).Count
       - 2);

   function Temporary_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Positive)
      return Natural
   is (Translator.Category_Register
       (Temporary, Offset - 1));

   function Scratch_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Natural)
      return Natural
   is (Translator.Category_Register
       (Scratch, Offset));

   function Argument_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Argument_Offset)
      return String
   is (Translator.Register_Image
       (Translator.Argument_Register (Offset)));

   function Local_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Local_Offset)
      return String
   is (Register_Image
       (Translator, Local_Register (Translator, Offset)));

   function Result_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Positive)
      return String
   is (Translator.Register_Image
       (Translator.Result_Register (Offset)));

   function Return_Value_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is (Translator.Register_Image
       (Translator.Return_Value_Register));

   function Stack_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is (Translator.Register_Image
       (Translator.Stack_Register));

   function Temporary_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Positive)
      return String
   is (Translator.Register_Image
       (Translator.Temporary_Register (Offset)));

   function Scratch_Register
     (Translator : Aqua32_Translator'Class;
      Offset     : Natural)
      return String
   is (Translator.Register_Image
       (Translator.Scratch_Register (Offset)));

   function Zero_Register
     (Translator : Aqua32_Translator'Class)
      return Natural
   is (Translator.Category_Register
       (Utility, 0));

   function Jump_Register
     (Translator : Aqua32_Translator'Class)
      return Natural
   is (Translator.Category_Register
       (Utility, 1));

   function Zero_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is (Translator.Register_Image (Translator.Zero_Register));

   function Jump_Register
     (Translator : Aqua32_Translator'Class)
      return String
   is (Translator.Register_Image (Translator.Jump_Register));

   function To_Src
     (Translator : in out Aqua32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand)
     return String
   is (Translator.To_String (Item, True));

   function To_Dst
     (Translator : in out Aqua32_Translator'Class;
      Item       : Tagatha.Transfers.Transfer_Operand)
      return String
   is (Translator.To_String (Item, False));

end Tagatha.Code.Aqua32;
