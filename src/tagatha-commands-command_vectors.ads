with Ada.Containers.Vectors;

package Tagatha.Commands.Command_Vectors is
  new Ada.Containers.Vectors (Positive, Tagatha_Command);
