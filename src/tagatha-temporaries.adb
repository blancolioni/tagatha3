package body Tagatha.Temporaries is

   ---------------------
   -- Assign_Register --
   ---------------------

   procedure Assign_Register (Item : in out Temporary;
                              R    : in     Positive)
   is
   begin
      Item.Register := R;
   end Assign_Register;

   ---------------------
   -- First_Reference --
   ---------------------

   function First_Reference (Item : Temporary) return Positive is
   begin
      return Item.First_Ref;
   end First_Reference;

   ------------------
   -- Get_Register --
   ------------------

   function Get_Register (Item : Temporary) return Natural is
   begin
      return Item.Register;
   end Get_Register;

   --------------------
   -- Last_Reference --
   --------------------

   function Last_Reference (Item : Temporary) return Positive is
   begin
      return Item.Last_Ref;
   end Last_Reference;

   ----------------
   -- New_Source --
   ----------------

   function New_Source return Temporary_Source is
   begin
      return new Natural'(0);
   end New_Source;

   --------------------
   -- Next_Temporary --
   --------------------

   function Next_Temporary (Source : in Temporary_Source)
                           return Temporary
   is
   begin
      Source.all := Source.all + 1;
      return new Temporary_Record'(Source.all, 0, 0, 0);
   end Next_Temporary;

   ----------------------
   -- Record_Reference --
   ----------------------

   procedure Record_Reference (Item    : in out Temporary;
                               Address : in     Positive)
   is
   begin
      if Item.First_Ref = 0 or else Address < Item.First_Ref then
         Item.First_Ref := Address;
      end if;
      if Item.Last_Ref < Address then
         Item.Last_Ref := Address;
      end if;
   end Record_Reference;

   ----------
   -- Show --
   ----------

   function Show (Item : Temporary) return String is
   begin
      return "t" & Integer'Image (-Item.Index);
   end Show;

end Tagatha.Temporaries;
