with Zstandard.Functions; use Zstandard.Functions;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_Ada is
begin
   if Argument_Count < 2 then
      Put_Line ("Zstandard version: " & Zstd_Version);
      Put_Line ("usage:");
      Put_Line (Command_Name & " <file#1> [additional files] <dictionary>");
      return;
   end if;

   for z in 1 .. Argument_Count loop
      if not Exists (Argument (z)) then
         Put_Line ("File '" & Argument (z) & "' does not exist, aborting.");
         return;
      end if;
   end loop;

   declare
      dictionary : String renames Argument (Argument_Count);
      srcsize   : Zstandard.Functions.File_Size;
      dstsize   : Zstandard.Functions.File_Size;
      goodcomp  : Boolean;
      gooddict  : Boolean;
      units     : Natural;
      tenths    : Natural;
      tenthstr  : String (1 .. 2);
      digest    : Compression_Dictionary;
   begin
      digest := Create_Compression_Dictionary_From_File (sample_file => dictionary,
                                                         quality     => 3,
                                                         successful  => gooddict);
      if not gooddict then
         Put_Line ("Failed to load dictionary");
         return;
      end if;

      for z in 1 .. Argument_Count - 1 loop
         declare
            orig_file : String renames Argument (z);
            dest_file : String := orig_file & ".zst";
            error_msg : String := Compress_File (source_file => orig_file,
                                                 output_file => dest_file,
                                                 digest      => digest,
                                                 source_size => srcsize,
                                                 output_size => dstsize,
                                                 successful  => goodcomp);
         begin
            if goodcomp then
               units := Natural (100 * dstsize / srcsize);
               tenths := (Natural (10 * dstsize / srcsize)) mod 10;
               tenthstr := tenths'Img;
               Put_Line ("");
               Put_Line ("   original file size:" & srcsize'Img);
               Put_Line (" compressed file size:" & dstsize'Img);
               Put_Line ("percentage compressed:" & units'Img & "." & tenthstr (2 ..2));
               Put_Line ("             new file: " & dest_file);
            else
               Put_Line (error_msg);
            end if;
         end;
      end loop;
      Destroy_Compression_Dictionary (digest);
   end;

end Demo_Ada;
