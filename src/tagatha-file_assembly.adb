package body Tagatha.File_Assembly is

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Assembly_Type) is
   begin
      Ada.Text_IO.Close (File.File);
   end Close;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Assembly_Type;
                   Name : in     String)
   is
   begin
      Ada.Text_IO.Create (File.File, Ada.Text_IO.Out_File, Name);
   end Open;

   --------------
   -- Put_Line --
   --------------

   overriding
   procedure Put_Line (A    : in out File_Assembly_Type;
                       Line : in String)
   is
   begin
      Ada.Text_IO.Put_Line (A.File, Line);
   end Put_Line;

end Tagatha.File_Assembly;
