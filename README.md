archive
===

This is a personal tool to make it easier to archive photos and other data to Amazon S3. In particular, it:

1. Takes a source directory and destination directory.
1. Creates a tar file of the source, placing that in the destination.
1. Drops a content listing (.manifest) for inclusion with the archive.
1. Inspects the content of the source, deciding if bzip2 compression is appropriate. This decision is made based on the percentage of content that is JPG, CR2, PNG, MP3, MOV, GP3, or other pre-compressed data.
1. Generates a PAR2 for the main file.
1. Splits the archive into blocks of 50MB (unless another size is specified).
1. Emits a bash script for recombining the split pieces and then attempting a repair using PAR2. (If the file is damaged, this fixes it; if it is undamaged, then this does nothing. The damaged file is renamed by PAR2 before dropping the corrected version in place.)

This yields one or more files that are ready for upload to S3. The archive is suitable for Glacier storage, while the manifest provides a small file that can be quickly searched for rudimentary information (dates, filenames).

compiling
===
raco exe archive.rkt

I'm not 100% sure if that is a distributable binary, but it works for my purposes.

usage
===

  $ archive -h
  
  archive [ <option> ... ] <src> <dst>
   where <option> is one of
  / -b <bs>, --block-size-bytes <bs> : Size of archive blocks (in bytes)
  \ -m <mbs>, --block-size-mb <mbs> : Size of archive blocks (in megabytes)
    -t <t>, --tag <t> : Identifier tag for archive.
    -y <y>, --year <y> : Year data was generated
    -r <r>, --redundancy <r> : Percent redundancy in PAR2 files.
    --threshold <th> : Percent compressible in a tree before we bzip. 0.75 is the default.
    -c, --compress : Compress with BZIP2.
    --help, -h : Show this help
    -- : Do not treat any remaining argument as a switch (at this level)
   /|\ Brackets indicate mutually exclusive options.
   Multiple single-letter switches can be combined after one `-'; for
    example: `-h-' is the same as `-h --'
