private with Ada.Containers.Vectors;
private with WL.String_Maps;

private with Tagatha.Expressions;
private with Tagatha.Temporaries;

with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Transfers;
with Tagatha.Transfers.Transfer_Vectors;

package Tagatha.Registry is

   type Tagatha_Registry is tagged private;

   procedure Start (Register    : in out Tagatha_Registry;
                    Unit_Label  : in     Tagatha.Labels.Tagatha_Label;
                    Size        : in     Natural);

   procedure Record_Push (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size;
                          Operand  : in     Tagatha.Operands.Tagatha_Operand);

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Tagatha.Operands.Tagatha_Operand);

   procedure Record_Drop (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size);

   procedure Record_Duplicate
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size);

   procedure Record_Swap (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size);

   procedure Record_Store
     (Register : in out Tagatha_Registry;
      Size     : in     Tagatha_Size);

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator);

   procedure Record_Native_Operation
     (Register          : in out Tagatha_Registry;
      Name              : String;
      Changed_Registers : String;
      Input_Words       : Natural;
      Output_Words      : Natural);

   procedure Record_Call
     (Register       : in out Tagatha_Registry;
      Subroutine     : in     Tagatha.Labels.Tagatha_Label;
      Argument_Count : Natural;
      Result_Count   : Natural);

   procedure Record_Loop (Register   : in out Tagatha_Registry;
                          Limit      : in     Local_Offset;
                          Counter    : in     Local_Offset;
                          End_Label  : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Jump (Register    : in out Tagatha_Registry;
                          Condition   : in     Tagatha_Condition;
                          Destination : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Label
     (Register   : in out Tagatha_Registry;
      Label      : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Location (Register : in out Tagatha_Registry;
                              Line     : in     Positive;
                              Column   : in     Positive);

   procedure Get_Transfers
     (Register  : in Tagatha_Registry;
      Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector);

private

   type Expression_Record is
      record
         Expression     : Tagatha.Expressions.Expression;
         Transfer_Index : Positive;
         Label          : Tagatha.Labels.Tagatha_Label;
      end record;

   package Expression_Vectors is
     new Ada.Containers.Vectors (Positive, Expression_Record);

   type Transfer_Record is
      record
         Transfer     : Tagatha.Transfers.Transfer;
         Source_Index : Positive;
      end record;

   package Transfer_Vectors is
     new Ada.Containers.Vectors (Positive, Transfer_Record);

   package Shelf_Maps is
     new WL.String_Maps (Tagatha.Temporaries.Temporary,
                         Tagatha.Temporaries."=");

   type Tagatha_Registry is tagged
      record
         Frame_Size    : Natural;
         Unit_Label    : Tagatha.Labels.Tagatha_Label;
         Stack         : Expression_Vectors.Vector;
         Transfers     : Transfer_Vectors.Vector;
         Temps         : Tagatha.Temporaries.Temporary_Source;
         Current_Label : Tagatha.Labels.Tagatha_Label;
         Default_Shelf : Tagatha.Temporaries.Temporary;
         Named_Shelves : Shelf_Maps.Map;
         Last_Line     : Natural := 0;
         Last_Column   : Natural := 0;
         Push_Index    : Natural := 0;
      end record;

end Tagatha.Registry;
