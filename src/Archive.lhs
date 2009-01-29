> module Archive (tarFixPaths, zipFixPaths) where

> import Codec.Archive.Tar
> import Codec.Archive.Zip
> import Project

Abstract out the physical location of the project.

> tarFixPaths archive@(TarArchive { archiveEntries = entries }) =
>     archive { archiveEntries = map fixEntry entries }
>   where
>     fixEntry entry@(TarEntry { entryHeader = header }) =
>         entry { entryHeader = fixHeader header }
>     fixHeader header@(TarHeader { tarFileName = path }) =
>         header { tarFileName = drop (length root) path }

Abstract out the physical location of the project.

> zipFixPaths archive@(Archive { zEntries = entries }) =
>     archive { zEntries = map fixEntry entries }
>   where
>     fixEntry entry@(Entry { eRelativePath = path }) =
>         entry { eRelativePath = drop (length root + 1) path }
