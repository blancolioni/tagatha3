package body Tagatha.Labels is

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     : in out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Name      : in     String;
                           Index     : in     Natural;
                           Segment   : in     Tagatha_Segment;
                           Location  : in     Positive;
                           Export    : in     Boolean := False);

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Name    : in     String;
                         Index   : in     Natural;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean);

   ------------------
   -- Create_Label --
   ------------------

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     : in out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Name      : in     String;
                           Index     : in     Natural;
                           Segment   : in     Tagatha_Segment;
                           Location  : in     Positive;
                           Export    : in     Boolean := False)
   is
      Exists    : Boolean;
   begin
      Find_Label (In_List, Name, Index, Label, Exists);
      if Exists then
         if Label.Has_Location then
            raise Constraint_Error with
            "Label '" & Name & "' defined twice at" &
            Positive'Image (Label.Location) & " and" &
            Positive'Image (Location);
         end if;
         Label.Segment := Segment;
         Label.Location := Location;
         Label.Has_Location := True;
         Label.Linked_To := Linked_To;
      else
         Label := new Tagatha_Label_Record'
           (Name         => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Index        => Index,
            Linked_To    => Linked_To,
            Segment      => Segment,
            Location     => Location,
            Exported     => Export,
            Imported     => False,
            Has_Location => True,
            Absolute     => False);
         In_List.Labels.Append (Label);
      end if;

   end Create_Label;

   ------------------
   -- Create_Label --
   ------------------

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     :    out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Name      : in     String;
                           Segment   : in     Tagatha_Segment;
                           Location  : in     Positive;
                           Export    : in     Boolean := False)
   is
   begin
      Create_Label (In_List, Label, Linked_To, Name,
                    0, Segment, Location, Export);
   end Create_Label;

   ------------------
   -- Create_Label --
   ------------------

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     :    out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Index     : in     Positive;
                           Location  : in     Positive)
   is
   begin
      Create_Label (In_List, Label, Linked_To, "",
                    Index, Executable, Location, False);
   end Create_Label;

   ----------------
   -- Find_Label --
   ----------------

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Name    : in     String;
                         Index   : in     Natural;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      Label   := null;
      Success := False;
      for I in 1 .. In_List.Labels.Last_Index loop
         Label := In_List.Labels.Element (I);
         if (Index = 0 and then Label.Name = Name) or else
           (Index > 0 and then Label.Index = Index)
         then
            Success := True;
            exit;
         end if;
      end loop;

   end Find_Label;

   ----------------
   -- Find_Label --
   ----------------

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Name    : in     String;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean)
   is
   begin
      Find_Label (In_List, Name, 0, Label, Success);
   end Find_Label;

   ----------------
   -- Find_Label --
   ----------------

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Index   : in     Positive;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean)
   is
   begin
      Find_Label (In_List, "", Index, Label, Success);
   end Find_Label;

   ---------------
   -- Has_Label --
   ---------------

   function Has_Label (Label : Tagatha_Label) return Boolean is
   begin
      return Label /= No_Label;
   end Has_Label;

   -------------
   -- Link_To --
   -------------

   procedure Link_To (First_Label : Tagatha_Label;
                      Next_Label  : Tagatha_Label)
   is
   begin
      if Next_Label /= null then
         declare
            Label : Tagatha_Label := First_Label;
         begin
            while Label.Linked_To /= null loop
               Label := Label.Linked_To;
            end loop;
            Label.Linked_To := Next_Label;
         end;
      end if;
   end Link_To;

   -----------------------
   -- Next_Linked_Label --
   -----------------------

   function Next_Linked_Label (Item : Tagatha_Label) return Tagatha_Label is
   begin
      return Item.Linked_To;
   end Next_Linked_Label;

   ---------------------
   -- Reference_Label --
   ---------------------

   procedure Reference_Label (In_List : in out Tagatha_Label_List;
                              Label   :    out Tagatha_Label;
                              Name    : in     String;
                              Import  : in     Boolean := False)
   is
      Success : Boolean;
   begin
      Find_Label (In_List, Name, Label, Success);
      if not Success then
         declare
            use Ada.Strings.Unbounded;
         begin
            Label := new Tagatha_Label_Record'
              (Name         => To_Unbounded_String (Name),
               Index        =>
                 (if Import or else Name /= "" then 0
                  else In_List.Labels.Last_Index + 1),
               Linked_To    => null,
               Segment      => Executable,
               Location     => 0,
               Exported     => False,
               Imported     => Import,
               Has_Location => False,
               Absolute     => False);
            In_List.Labels.Append (Label);
         end;
      end if;
   end Reference_Label;

   ---------------------
   -- Reference_Label --
   ---------------------

   procedure Reference_Label (In_List : in out Tagatha_Label_List;
                              Label   :    out Tagatha_Label;
                              Index   : in     Positive)
   is
      Success : Boolean;
   begin
      Find_Label (In_List, Index, Label, Success);
      if not Success then
         declare
            use Ada.Strings.Unbounded;
         begin
            Label := new Tagatha_Label_Record'
              (Name         => Null_Unbounded_String,
               Index        => Index,
               Linked_To    => null,
               Segment      => Executable,
               Location     => 0,
               Exported     => False,
               Imported     => False,
               Has_Location => False,
               Absolute     => False);
            In_List.Labels.Append (Label);
         end;
      end if;
   end Reference_Label;

   ----------
   -- Show --
   ----------

   function Show (Item         : Tagatha_Label;
                  Local_Prefix : Character)
                 return String
   is
   begin
      if Item.Index > 0 then
         declare
            Result : String := Positive'Image (Item.Index);
         begin
            Result (Result'First) := Local_Prefix;
            return Result;
         end;
      else
         return Ada.Strings.Unbounded.To_String (Item.Name);
      end if;
   end Show;

   --------------
   -- Show_All --
   --------------

   function Show_All
     (Item         : Tagatha_Label;
      Local_Prefix : Character)
      return String
   is
      use Ada.Strings.Unbounded;
      Label : Tagatha_Label := Item;
      Label_Text : Unbounded_String;
   begin
      while Has_Label (Label) loop
         Label_Text := Label_Text & Show (Label, Local_Prefix) & ":";
         Label := Next_Linked_Label (Label);
      end loop;
      return To_String (Label_Text);
   end Show_All;

end Tagatha.Labels;
