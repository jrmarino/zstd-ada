with Zstandard.Functions; use Zstandard.Functions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_Ada
is
   message : constant String :=
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus " &
      "tempor erat quis metus faucibus, a elementum odio varius. Donec " &
      "ultrices posuere nisl. Aliquam molestie, nibh a ultrices dictum, " &
      "neque nisi pellentesque sapien, a molestie urna quam eu leo. Morbi " &
      "nec finibus odio, vel maximus lorem. Proin eget viverra tellus, eu " &
      "vestibulum est. Aliquam pharetra vulputate porttitor. Integer eu " &
      "varius dui. Vivamus non metus id metus cursus auctor. Integer erat " &
      "augue, pharetra in nisl a, aliquet tempor leo.";
   vessel : String := (1 .. message'Length => ' ');
begin
   Put_Line ("Zstandard version: " & Zstd_Version);

   Put_Line ("");
   Put_Line ("message:");
   Put_Line (message);

   declare
      nominal   : Boolean;
      compacted : constant String := Compress (source_data => message,
                                               successful  => nominal,
                                               quality     => 1);
   begin
      if not nominal then
         Put_Line ("FAILURE!");
         Put_Line (compacted);
         return;
      end if;

      Put_Line ("");
      Put_Line ("  original length:" & message'Length'Img);
      Put_Line ("compressed length:" & compacted'Length'Img);
      Put_Line ("");
      Put_Line ("Testing decompression ...");
      vessel := Decompress (source_data => compacted,
                            successful  => nominal);
      if not nominal then
         Put_Line ("FAILURE!");
         Put_Line (vessel);
         return;
      end if;
      if message = vessel then
         Put_Line ("SUCCESS!  Decompressed text is the same as the original");
      else
         Put_Line ("ERROR!  Return value different");
         Put_Line (vessel);
      end if;
   end;
end Demo_Ada;
