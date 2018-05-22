with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Tagatha.Labels is

   type Tagatha_Label is private;

   type Tagatha_Label_List is private;

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     :    out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Name      : in     String;
                           Segment   : in     Tagatha_Segment;
                           Location  : in     Positive;
                           Export    : in     Boolean := False);

   procedure Create_Label (In_List   : in out Tagatha_Label_List;
                           Label     :    out Tagatha_Label;
                           Linked_To : in     Tagatha_Label;
                           Index     : in     Positive;
                           Location  : in     Positive);

   procedure Reference_Label (In_List : in out Tagatha_Label_List;
                              Label   :    out Tagatha_Label;
                              Name    : in     String;
                              Import  : in     Boolean := False);

   procedure Reference_Label (In_List : in out Tagatha_Label_List;
                              Label   :    out Tagatha_Label;
                              Index   : in     Positive);

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Name    : in     String;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean);

   procedure Find_Label (In_List : in     Tagatha_Label_List;
                         Index   : in     Positive;
                         Label   :    out Tagatha_Label;
                         Success :    out Boolean);

   procedure Link_To (First_Label : Tagatha_Label;
                      Next_Label  : Tagatha_Label);

   function Next_Linked_Label (Item : Tagatha_Label) return Tagatha_Label;

   function Show (Item         : Tagatha_Label;
                  Local_Prefix : Character)
                 return String;

   function Exported
     (Item         : Tagatha_Label)
      return Boolean;

   function Show_All
     (Item         : Tagatha_Label;
      Local_Prefix : Character)
      return String;

   function Has_Label (Label : Tagatha_Label) return Boolean;
   function Is_Local (Label : Tagatha_Label) return Boolean;

   No_Label : constant Tagatha_Label;

private

   type Tagatha_Label_Record is
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Index        : Natural;
         Linked_To    : Tagatha_Label;
         Location     : Natural;
         Segment      : Tagatha_Segment;
         Absolute     : Boolean;
         Has_Location : Boolean;
         Exported     : Boolean;
         Imported     : Boolean;
      end record;

   type Tagatha_Label is access Tagatha_Label_Record;

   function Exported
     (Item         : Tagatha_Label)
      return Boolean
   is (Item.Exported);

   function Is_Local (Label : Tagatha_Label) return Boolean
   is (Label.Index > 0);

   No_Label : constant Tagatha_Label := null;

   package Label_Vector is
      new Ada.Containers.Vectors (Positive, Tagatha_Label);

   type Tagatha_Label_List is
      record
         Labels  : Label_Vector.Vector;
      end record;

end Tagatha.Labels;
