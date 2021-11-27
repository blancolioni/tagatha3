with Ada.Strings.Unbounded;

package Tagatha.Code.X86_64 is

   type X86_64_Translator is new Standard_Translator (64) with private;

   overriding
   procedure Start (T      : in out X86_64_Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean);

   overriding
   procedure Finish (T   : in out X86_64_Translator;
                     Asm : in out Assembly'Class);

   overriding
   procedure Encode (T    : in out X86_64_Translator;
                     Asm  : in out Assembly'Class;
                     Item : in     Tagatha.Transfers.Transfer);

   overriding
   procedure Label (T     : in out X86_64_Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label);

   overriding
   procedure Begin_Frame (T           : in out X86_64_Translator;
                          Asm         : in out Assembly'Class;
                          Return_Count    : Natural;
                          Arg_Count       : Natural;
                          Local_Count     : Natural;
                          Temporary_Count : Natural);

   overriding
   function General_Registers (T : X86_64_Translator) return Positive;

   overriding
   function Word_Size (T : X86_64_Translator) return Tagatha_Size;

   function Get_Translator return Translator'Class;

   overriding
   procedure File_Preamble (T                : in out X86_64_Translator;
                            Asm              : in out Assembly'Class;
                            Source_File_Name : in     String);

private

   type X86_64_Translator is new Standard_Translator (64) with
      record
         Current_Unit_Name : Ada.Strings.Unbounded.Unbounded_String;
         Reverse_Test      : Boolean;
         Arg_Size          : Natural;
         Frame_Size        : Natural;
      end record;

end Tagatha.Code.X86_64;
