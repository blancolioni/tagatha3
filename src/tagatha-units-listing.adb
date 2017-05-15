with Ada.Text_IO;

package body Tagatha.Units.Listing is

   ---------------------------
   -- Write_Command_Listing --
   ---------------------------

   procedure Write_Command_Listing
     (Unit : Tagatha_Unit)
   is
      use Ada.Text_IO;
      Path : constant String :=
               Unit.File_System_Name & "-commands.lst";
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Set_Output (File);
      for Sub of Unit.Subprograms loop
         Put_Line (Subprogram_Name (Sub) & ":");
         for Command of Sub.Executable_Segment loop
            Put_Line (Tagatha.Commands.Show (Command));
         end loop;
      end loop;
      Set_Output (Standard_Output);
      Close (File);
   end Write_Command_Listing;

   ----------------------------
   -- Write_Transfer_Listing --
   ----------------------------

   procedure Write_Transfer_Listing
     (Unit : Tagatha_Unit)
   is
      use Ada.Text_IO;
      Path : constant String :=
               Unit.File_System_Name & "-transfers.lst";
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Set_Output (File);
      for Sub of Unit.Subprograms loop
         Put_Line (Subprogram_Name (Sub) & ":");
         for Transfer of Sub.Transfers loop
            Put_Line (Tagatha.Transfers.Show (Transfer));
         end loop;
      end loop;
      Set_Output (Standard_Output);
      Close (File);
   end Write_Transfer_Listing;

end Tagatha.Units.Listing;
