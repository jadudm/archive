archive
===

This is a personal tool to make it easier to archive photos and other data to Amazon S3. In particular, it:

1. Takes a source directory and destination directory.
1. Creates a tar file of the source, placing that in the destination.
1. Inspects the content of the source, deciding if bzip2 compression is appropriate. This decision is made based on the percentage of content that is JPG, CR2, PNG, MP3, MOV, GP3, or other pre-compressed data.
1. Splits the archive into blocks of 500MB (unless another size is specified).
1. Generates a PAR2 archive for each 500MB block.
1. Emits two bash scripts: one for validating the archive, and one for re-assembling the archive.
1. Drops a content listing (.manifest) for inclusion with the archive.

This yields one or more files that are ready for upload to S3. The archive is suitable for Glacier storage, while the manifest provides a small file that can be quickly searched for rudimentary information (dates, filenames).
