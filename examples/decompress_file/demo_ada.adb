with Zstandard.Functions; use Zstandard.Functions;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_Ada is
begin
   if Argument_Count /= 1 then
      Put_Line ("Zstandard version: " & Zstd_Version);
      Put_Line ("usage:");
      Put_Line (Command_Name & " <path/to/file.zst>");
      return;
   end if;

   declare
      path2file : String renames Argument (1);
      namelen   : constant Natural := path2file'Length;
      nlast     : constant Natural := path2file'Last;
   begin
      if not Exists (path2file) then
         Put_Line ("File '" & path2file & "' does not exist, aborting.");
         return;
      else
         if namelen < 5 or else
           path2file (nlast - 3 .. nlast) /= ".zst"
         then
            Put_Line ("File name doesn't have the .zst extension, aborting.");
            return;
         end if;
      end if;
   end;

   declare
      type Real4 is digits 4;
      path2file : String renames Argument (1);
      nlast     : constant Natural := path2file'Last;
      decmpfile : String := path2file (path2file'First .. nlast - 4);
      srcsize   : Zstandard.Functions.File_Size;
      dstsize   : Zstandard.Functions.File_Size;
      gooddecmp : Boolean;
      error_msg : String := Decompress_File (source_file => path2file,
                                             output_file => decmpfile,
                                             source_size => srcsize,
                                             output_size => dstsize,
                                             successful  => gooddecmp);
   begin
      if gooddecmp then
         Put_Line ("   original file size:" & srcsize'Img);
         Put_Line (" compressed file size:" & dstsize'Img);
         Put_Line ("             new file: " & decmpfile);
      else
         Put_Line (error_msg);
      end if;
   end;

end Demo_Ada;
