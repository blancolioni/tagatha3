private with Ada.Text_IO;

with Tagatha.Code;

package Tagatha.File_Assembly is

   type File_Assembly_Type is limited new Tagatha.Code.Assembly with private;

   overriding
   procedure Put_Line (A    : in out File_Assembly_Type;
                       Line : in     String);

   procedure Open (File : in out File_Assembly_Type;
                   Name : in     String);
   procedure Close (File : in out File_Assembly_Type);

private

   type File_Assembly_Type is limited new Tagatha.Code.Assembly with
      record
         File : Ada.Text_IO.File_Type;
      end record;

end Tagatha.File_Assembly;
