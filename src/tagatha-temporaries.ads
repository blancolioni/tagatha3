package Tagatha.Temporaries is

   type Temporary is private;

   type Temporary_Source is private;

   function New_Source return Temporary_Source;

   function Next_Temporary (Source : in Temporary_Source)
                           return Temporary;

   function Show (Item : Temporary) return String;

   function First_Reference (Item : Temporary) return Positive;
   function Last_Reference (Item : Temporary) return Positive;

   procedure Record_Reference (Item    : in out Temporary;
                               Address : in     Positive);

   procedure Assign_Register (Item : in out Temporary;
                              R    : in     Positive);

   --  A value of 0 for the register indicates that it has none
   function Get_Register (Item : Temporary) return Natural;

private

   type Temporary_Source is access Natural;
   type Temporary_Record is
      record
         Index               : Positive;
         First_Ref, Last_Ref : Natural;
         Register            : Natural;
      end record;

   type Temporary is access Temporary_Record;

end Tagatha.Temporaries;
