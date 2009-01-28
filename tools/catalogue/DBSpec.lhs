#! /usr/bin/env runhaskell

> import Database.HaskellDB.DBLayout
> import Database.HaskellDB.DBSpec.DBSpecToDBDirect

> database = DBInfo {
>     dbname = "gbtarchive"
>   , opts   = DBOptions { useBString = False }
>   , tbls   = [
>         detectors_tbl
>       , observations_tbl
>       , observers_tbl
>       , polarization_types_tbl
>       , polarizations_tbl
>       , projects_tbl
>       , receivers_tbl
>       , sources_tbl
>       , telescopes_tbl
>       , windows_tbl
>       ]
>   }

> detectors_tbl = TInfo {
>     tname = "detectors"
>   , cols  = [
>         CInfo { cname = "id",   descr = (IntT,    True)  }
>       , CInfo { cname = "name", descr = (StringT, False) }
>       ]
>   }

> observations_tbl = TInfo {
>     tname = "observations"
>   , cols  = [
>         CInfo { cname = "id",          descr = (IntT,    True)  }
>       , CInfo { cname = "project_id",  descr = (IntT,    False) }
>       , CInfo { cname = "source_id",   descr = (IntT,    False) }
>       , CInfo { cname = "scan",        descr = (IntT,    False) }
>       , CInfo { cname = "datetime",    descr = (StringT, False) }
>       , CInfo { cname = "exposure",    descr = (IntT,    False) }
>       , CInfo { cname = "ra2000",      descr = (DoubleT, False) }
>       , CInfo { cname = "dec2000",     descr = (DoubleT, False) }
>       , CInfo { cname = "filesize",    descr = (IntT,    False) }
>       ]
>   }

> observers_tbl = TInfo {
>     tname = "observers"
>   , cols  = [
>         CInfo { cname = "id",   descr = (IntT,    True)  }
>       , CInfo { cname = "name", descr = (StringT, False) }
>       ]
>   }

> polarization_types_tbl = TInfo {
>     tname = "polarization_types"
>   , cols  = [
>         CInfo { cname = "id",   descr = (IntT,    True)  }
>       , CInfo { cname = "name", descr = (StringT, False) }
>       ]
>   }

> polarizations_tbl = TInfo {
>     tname = "polarizations"
>   , cols  = [
>         CInfo { cname = "id",                   descr = (IntT,    True)  }
>       , CInfo { cname = "polarization_type_id", descr = (IntT,    False) }
>       , CInfo { cname = "name",                 descr = (StringT, False) }
>       ]
>   }

> projects_tbl = TInfo {
>     tname = "projects"
>   , cols  = [
>         CInfo { cname = "id",           descr = (IntT,    True)  }
>       , CInfo { cname = "observer_id",  descr = (IntT, False)    }
>       , CInfo { cname = "telescope_id", descr = (IntT, False)    }
>       , CInfo { cname = "name",         descr = (StringT, False) }
>       ]
>   }

> receivers_tbl = TInfo {
>     tname = "receivers"
>   , cols  = [
>         CInfo { cname = "id",       descr = (IntT,    True)  }
>       , CInfo { cname = "name",     descr = (StringT, False) }
>       , CInfo { cname = "min_freq", descr = (DoubleT, False) }
>       , CInfo { cname = "max_freq", descr = (DoubleT, False) }
>       , CInfo { cname = "band",     descr = (StringT, True)  }
>       ]
>   }

> sources_tbl = TInfo {
>     tname = "sources"
>   , cols  = [
>         CInfo { cname = "id",   descr = (IntT,    True)  }
>       , CInfo { cname = "name", descr = (StringT, False) }
>       ]
>   }

> telescopes_tbl = TInfo {
>     tname = "telescopes"
>   , cols  = [
>         CInfo { cname = "id",   descr = (IntT,    True)  }
>       , CInfo { cname = "name", descr = (StringT, False) }
>       ]
>   }

> windows_tbl = TInfo {
>     tname = "windows"
>   , cols  = [
>         CInfo { cname = "id",              descr = (IntT,    True)  }
>       , CInfo { cname = "detector_id",     descr = (IntT,    False) }
>       , CInfo { cname = "observation_id",  descr = (IntT,    False) }
>       , CInfo { cname = "polarization_id", descr = (IntT,    False) }
>       , CInfo { cname = "receiver_id",     descr = (IntT,    False) }
>       , CInfo { cname = "frequency",       descr = (DoubleT, False) }
>       , CInfo { cname = "bandwidth",       descr = (DoubleT, False) }
>       ]
>   }

> main = dbInfoToModuleFiles "." "DBInfo" database
