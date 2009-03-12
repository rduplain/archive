-- | A Haskell wrapper for CFITSIO.  See http://heasarc.nasa.gov/docs/software/fitsio/fitsio.html
module FitsIO where

-- For the FFI, we have to use the 8-char Fortran style names.  Alas!
#include "fitsio.h"

import C2HS
import Control.Monad
import Control.Monad.Trans
import Data.Char

#c
/* These all include space for the terminating NULL. */
enum FLen {
    FLenFileName = FLEN_FILENAME  /* max length of a filename  */
  , FLenKeyword  = FLEN_KEYWORD   /* max length of a keyword (HIERARCH convention) */
  , FLenCard     = FLEN_CARD      /* length of a FITS header card */
  , FLenValue    = FLEN_VALUE     /* max length of a keyword value string */
  , FLenComment  = FLEN_COMMENT   /* max length of a keyword comment string */
  , FLenErrMsg   = FLEN_ERRMSG    /* max length of a FITSIO error message */
  , FLenStatus   = FLEN_STATUS    /* max length of a FITSIO status text string */
};
#endc

{# enum FLen {} #}

allocaFileName = allocaBytes (fromEnum FLenFileName)
allocaKeyword  = allocaBytes (fromEnum FLenKeyword)
allocaCard     = allocaBytes (fromEnum FLenCard)
allocaValue    = allocaBytes (fromEnum FLenValue)
allocaComment  = allocaBytes (fromEnum FLenComment)
allocaErrMsg   = allocaBytes (fromEnum FLenErrMsg)
allocaStatus   = allocaBytes (fromEnum FLenStatus)

#c
enum ColType {
    TBit        = TBIT         /* codes for FITS table data types */
  , TByte       = TBYTE
  , TSByte      = TSBYTE
  , TLogical    = TLOGICAL
  , TString     = TSTRING
  , TUShort     = TUSHORT
  , TShort      = TSHORT
  , TUInt       = TUINT
  , TInt        = TINT
  , TULong      = TULONG
  , TLong       = TLONG
  , TFloat      = TFLOAT
  , TLongLong   = TLONGLONG
  , TDouble     = TDOUBLE
  , TComplex    = TCOMPLEX
  , TDblComplex = TDBLCOMPLEX
};
#endc

{# enum ColType {} deriving (Show) #}

class Read a => FitsValue a where
  readKey     :: FitsFile -> String -> FitsIO (a, String)
  readKey f n = do
    (v, c) <- readKeyString f n
    return (read v, c)
    
  readCol :: FitsFile -> Int -> Int -> Int -> Int -> a -> FitsIO ([a], Int)

instance FitsValue Double where
  readKey = readKeyCDouble
  readCol = readColCDouble
  
instance FitsValue Float where
  readKey = readKeyCFloat
  readCol = readColCFloat

instance FitsValue Int where
  readKey = readKeyCInt
  readCol = readColCInt

instance FitsValue String where
  readKey f n = do
      (v, c) <- readKeyString f n
      return (trim v, c)
    where ltrim = dropWhile isSpace
          rtrim = reverse . ltrim . reverse
          trim  = rtrim . ltrim
  
  readCol = readColString

-- | The API is designed to thread a "status" (really, error) variable
-- through most routines.  These routines take an in/out status
-- parameter in last place and return status redundantly as the value
-- of the function.  C uses an int, newtype'd here for safety.

newtype Status = Status { status :: Int } deriving (Show)

-- | A monad to handle threading of status information.
newtype FitsIO a = FitsIO { runFitsIO :: Status -> IO (Status, a) }

runFits   :: FitsIO a -> IO a
runFits f = do
  (_, x) <- runFitsIO f noError
  return x

instance Functor FitsIO where
  fmap f m = FitsIO $ \s -> do
    (t, x) <- runFitsIO m s
    return (t, f x)

instance Monad FitsIO where
  return a = FitsIO $ \s -> return (s, a)
  m >>= k  = FitsIO $ \s -> do
    (t, x) <- runFitsIO m s
    runFitsIO (k x) t
  fail str = FitsIO $ \_ -> fail str

instance MonadIO FitsIO where
  liftIO f = FitsIO $ \s -> do
    x <- f
    return (s, x)

clearStatus :: FitsIO ()
clearStatus = setStatus noError

setStatus   :: Status -> FitsIO ()
setStatus s = FitsIO $ \_ -> return (s, ())

getStatus :: FitsIO Status
getStatus = FitsIO $ \s -> return (s, s)

fp   :: (Status -> IO (Status, a)) -> FitsIO a
fp f = FitsIO $ \s -> f s

f1 :: (b -> Status -> IO (Status, a)) -> b -> FitsIO a
f1 f b = fp $ f b

f2 :: (b -> c -> Status -> IO (Status, a)) -> b -> c -> FitsIO a
f2 f b c = fp $ f b c

f3 :: (b -> c -> d -> Status -> IO (Status, a)) -> b -> c -> d -> FitsIO a
f3 f b c d = fp $ f b c d

f4 :: (b -> c -> d -> e -> Status -> IO (Status, a)) -> b -> c -> d -> e -> FitsIO a
f4 f b c d e = fp $ f b c d e

r2             :: (a, b, c) -> IO (a, (b, c))
r2 (a,  b,  c) = return (a, (b, c))

r3              :: (a, b, c, d) -> IO (a, (b, c, d))
r3 (a, b, c, d) = return (a, (b, c, d))

r4                 :: (a, b, c, d, e) -> IO (a, (b, c, d, e))
r4 (a, b, c, d, e) = return (a, (b, c, d, e))

-- | Default status value at the start of a program.
noError :: Status
noError = Status 0

-- | Convert a C int to a status value.
cToStatus :: Integral a => a -> Status
cToStatus = Status . cIntConv

-- | Convert a status value to a C int.
cFromStatus :: Integral a => Status -> a
cFromStatus = cIntConv . status

-- | Pass a status value as an in/out parameter.
withStatusConv :: (Integral a, Storable a) => Status -> (Ptr a -> IO b) -> IO b
withStatusConv = with . cFromStatus

-- | Return a descriptive text string (30 char max.) corresponding to
-- a CFITSIO error status code.
getErrStatus   :: Status -> IO String
getErrStatus s =
  allocaStatus $ \str -> do
    {# call unsafe ffgerr #} (cFromStatus s) str
    peekCString str

-- | Return the top (oldest) 80-character error message from the
-- internal CFITSIO stack of error messages and shift any remaining
-- messages on the stack up one level. Call this routine repeatedly to
-- get each message in sequence. The function returns a value = 0 and
-- a null error message when the error stack is empty.
readErrMsg :: IO String
readErrMsg =
  allocaErrMsg $ \str -> do
    {# call unsafe ffgmsg #} str
    peekCString str

{# fun unsafe ffpmrk as writeErrMark {} -> `()' #}
{# fun unsafe ffcmrk as clearErrMark {} -> `()' #}
{# fun unsafe ffcmsg as clearErrMsg  {} -> `()' #}

{# pointer *fitsfile as FitsFile foreign newtype #}

foreign import ccall "CloseFile.h &closeFile"
  closeFile :: FunPtr (Ptr FitsFile -> IO ())

newFitsFile   :: Ptr (Ptr FitsFile) -> IO FitsFile
newFitsFile p = peek p >>= fmap FitsFile . newForeignPtr closeFile

{# fun unsafe ffdelt as deleteFile_
   { withFitsFile*   `FitsFile'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Return the name of the opened FITS file.
fileName = f1 fileName_

fileName_     :: FitsFile -> Status -> IO (Status, String)
fileName_ f s =
  allocaFileName $ \str -> do
    s''  <- withFitsFile f (\f' -> withStatusConv s (\s' -> {# call unsafe ffflnm #} f' str s'))
    str' <- peekCString str
    return (cToStatus s'', str')

#c
enum IOMode {
    ReadOnly  = READONLY
  , ReadWrite = READWRITE
};
#endc

{# enum IOMode {} deriving (Show) #}

-- | Return the I/O mode of the opened FITS file.
fileMode = f1 fileMode_
{# fun unsafe ffflmd as fileMode_
   { withFitsFile*   `FitsFile'
   , alloca-         `IOMode'   peekEnum*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

[openFile, openDiskFile, openData, openTable, openImage] =
  map f2 [openFile_, openDiskFile_, openData_, openTable_, openImage_]

-- | Open an existing data file.
{# fun unsafe ffopen as openFile_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , cFromEnum       `IOMode'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe ffdkopn as openDiskFile_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , cFromEnum       `IOMode'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe ffdopn as openData_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , cFromEnum       `IOMode'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe fftopn as openTable_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , cFromEnum       `IOMode'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe ffiopn as openImage_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , cFromEnum       `IOMode'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

[createFile, createDiskFile] = map f1 [createFile_, createDiskFile_]

-- | Create and open a new empty output FITS file.
{# fun unsafe ffinit as createFile_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe ffdkinit as createDiskFile_
   { alloca-         `FitsFile' newFitsFile*
   ,                 `String'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Return the total number of HDUs in the FITS file. This returns
-- the number of completely defined HDUs in the file. If a new HDU has
-- just been added to the FITS file, then that last HDU will only be
-- counted if it has been closed, or if data has been written to the
-- HDU. The current HDU remains unchanged by this routine.

getNumHdus :: FitsFile -> FitsIO Int
getNumHdus = f1 getNumHdus'

{# fun unsafe ffthdu as getNumHdus'
   { withFitsFile*   `FitsFile'
   , alloca-         `Int'      peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Return the number of the current HDU (CHDU) in the FITS file
-- (where the primary array = 1). This function returns the HDU number
-- rather than a status value.

getHduNum :: FitsFile -> FitsIO Int
getHduNum = liftIO . getHduNum'

{# fun unsafe ffghdn as getHduNum'
   { withFitsFile* `FitsFile'
   , alloca-       `Int'
   } -> `Int'
#}

#c
enum HduType {
    ImageHdu  = IMAGE_HDU   /* Primary Array or IMAGE HDU */
  , AsciiTbl  = ASCII_TBL   /* ASCII table HDU */
  , BinaryTbl = BINARY_TBL  /* Binary table HDU */
  , AnyHdu    = ANY_HDU     /* matches any HDU type */
};
#endc

{# enum HduType {} deriving (Eq, Show) #}

-- | Return the type of the current HDU in the FITS file. The possible
-- values for hdutype are: IMAGE_HDU, ASCII_TBL, or BINARY_TBL.

getHduType :: FitsFile -> FitsIO HduType
getHduType = f1 getHduType'

{# fun unsafe ffghdt as getHduType'
   { withFitsFile*   `FitsFile'
   , alloca-         `HduType' peekEnum*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

movAbsHdu = f2 movAbsHdu_
{# fun unsafe ffmahd as movAbsHdu_
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , alloca-         `HduType' peekEnum*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

movRelHdu :: FitsFile -> Int -> FitsIO HduType
movRelHdu = f2 movRelHdu'

{# fun unsafe ffmrhd as movRelHdu'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , alloca-         `HduType' peekEnum*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{# fun unsafe ffmnhd as movNamHdu_
   { withFitsFile*   `FitsFile'
   , cFromEnum       `HduType'
   ,                 `String'
   ,                 `Int'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Copy all or part of the HDUs in the FITS file associated with
-- infptr and append them to the end of the FITS file associated with
-- outfptr. If 'previous' is true (not 0), then any HDUs preceding the
-- current HDU in the input file will be copied to the output
-- file. Similarly, 'current' and 'following' determine whether the
-- current HDU, and/or any following HDUs in the input file will be
-- copied to the output file. Thus, if all 3 parameters are true, then
-- the entire input file will be copied. On exit, the current HDU in
-- the input file will be unchanged, and the last HDU in the output
-- file will be the current HDU.
{# fun unsafe ffcpfl as copyFile
   { withFitsFile*   `FitsFile'
   , withFitsFile*   `FitsFile'
   ,                 `Int'
   ,                 `Int'
   ,                 `Int'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Copy the current HDU from the FITS file associated with infptr
-- and append it to the end of the FITS file associated with
-- outfptr. Space may be reserved for MOREKEYS additional keywords in
-- the output header.
{# fun unsafe ffcopy as copyHdu
   { withFitsFile*   `FitsFile'
   , withFitsFile*   `FitsFile'
   ,                 `Int'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Copy the header (and not the data) from the CHDU associated with
-- infptr to the CHDU associated with outfptr. If the current output
-- HDU is not completely empty, then the CHDU will be closed and a new
-- HDU will be appended to the output file. An empty output data unit
-- will be created with all values initially = 0).
{# fun unsafe ffcphd as copyHeader
   { withFitsFile*   `FitsFile'
   , withFitsFile*   `FitsFile'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Delete the CHDU in the FITS file. Any following HDUs will be
-- shifted forward in the file, to fill in the gap created by the
-- deleted HDU. In the case of deleting the primary array (the first
-- HDU in the file) then the current primary array will be replace by
-- a null primary array containing the minimum set of required
-- keywords and no data. If there are more extensions in the file
-- following the one that is deleted, then the the CHDU will be
-- redefined to point to the following extension. If there are no
-- following extensions then the CHDU will be redefined to point to
-- the previous HDU. The output hdutype parameter returns the type of
-- the new CHDU. A null pointer may be given for hdutype if the
-- returned value is not needed.
{# fun unsafe ffdhdu as deleteHdu
   { withFitsFile*   `FitsFile'
   , alloca-         `HduType' peekEnum*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}


-- | Return the number of existing keywords (not counting the END
-- keyword) and the amount of space currently available for more
-- keywords. It returns morekeys = -1 if the header has not yet been
-- closed. Note that CFITSIO will dynamically add space if required
-- when writing new keywords to a header so in practice there is no
-- limit to the number of keywords that can be added to a header. A
-- null pointer may be entered for the morekeys parameter if it's
-- value is not needed.

getHdrSpace   :: FitsFile -> FitsIO (Int, Int)
getHdrSpace f = FitsIO $ \s -> getHdrSpace' f s >>= r2

{# fun unsafe ffghsp as getHdrSpace'
   { withFitsFile*   `FitsFile'
   , alloca-         `Int'        peekIntConv*
   , alloca-         `Int'        peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyString     :: FitsFile -> String -> FitsIO (String, String)
readKeyString f n = FitsIO $ \s -> readKeyString' f n s >>= r2

{# fun unsafe ffgkys as readKeyString'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , allocaValue-    `String'     peekCString*
   , allocaComment-  `String'     peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyCInt     :: FitsFile -> String -> FitsIO (Int, String)
readKeyCInt f n = FitsIO $ \s -> readKeyCInt' f n s >>= r2

{# fun unsafe ffgkyl as readKeyCInt'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , alloca-         `Int'        peekIntConv*
   , allocaComment-  `String'     peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyCFloat     :: FitsFile -> String -> FitsIO (Float, String)
readKeyCFloat f n = FitsIO $ \s -> readKeyCFloat' f n s >>= r2

{# fun unsafe ffgkye as readKeyCFloat'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , alloca-         `Float'      peekFloatConv*
   , allocaComment-  `String'     peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyCDouble     :: FitsFile -> String -> FitsIO (Double, String)
readKeyCDouble f n = FitsIO $ \s -> readKeyCDouble' f n s >>= r2

{# fun unsafe ffgkyd as readKeyCDouble'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , alloca-         `Double'     peekFloatConv*
   , allocaComment-  `String'     peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

{--
int ffgkyj(fitsfile *fptr, char *keyname, long *value, char *comm, int *status);
--}

-- | Return the nth header record in the CHU. The first keyword in the
-- header is at keynum = 1; if keynum = 0 then these routines simply
-- reset the internal CFITSIO pointer to the beginning of the header
-- so that subsequent keyword operations will start at the top of the
-- header (e.g., prior to searching for keywords using wild cards in
-- the keyword name). The first routine returns the entire
-- 80-character header record (with trailing blanks truncated), while
-- the second routine parses the record and returns the name, value,
-- and comment fields as separate (blank truncated) character
-- strings. If a NULL comment pointer is given on input, then the
-- comment string will not be returned.

readRecord :: FitsFile -> Int -> FitsIO String
readRecord = f2 readRecord'

{# fun unsafe ffgrec as readRecord'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , allocaCard-     `String' peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyN     :: FitsFile -> Int -> FitsIO (String, String, String)
readKeyN f n = FitsIO $ \s -> readKeyN' f n s >>= r3

{# fun unsafe ffgkyn as readKeyN'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , allocaKeyword-  `String' peekCString*
   , allocaValue-    `String' peekCString*
   , allocaComment-  `String' peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readCard :: FitsFile -> String -> FitsIO String
readCard = f2 readCard'

{# fun unsafe ffgcrd as readCard'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , allocaCard-     `String' peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

readKeyword     :: FitsFile -> String -> FitsIO (String, String)
readKeyword f k = FitsIO $ \s -> readKeyword' f k s >>= r2

{# fun unsafe ffgkey as readKeyword'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   , allocaValue-    `String' peekCString*
   , allocaComment-  `String' peekCString*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}


-- | Get the number of rows in the current FITS table.  The number of
-- rows is given by the NAXIS2 keyword.

getNumRows :: FitsFile -> FitsIO Int
getNumRows = f1 getNumRows'

{# fun unsafe ffgnrw as getNumRows'
   { withFitsFile*   `FitsFile'
   , alloca-         `Int'       peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Get the number of columns in the current FITS table.  The number
-- of rows is given by the TFIELDS keyword in the header of the table.

getNumCols :: FitsFile -> FitsIO Int
getNumCols = f1 getNumCols'

{# fun unsafe ffgncl as getNumCols'
   { withFitsFile*   `FitsFile'
   , alloca-         `Int'       peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

getColNum :: FitsFile -> Bool -> String -> FitsIO Int
getColNum = f3 getColNum'

{# fun unsafe ffgcno as getColNum'
   { withFitsFile*   `FitsFile'
   ,                 `Bool'
   ,                 `String'
   , alloca-         `Int'       peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

getColName                  :: FitsFile -> Bool -> String -> FitsIO (String, Int)
getColName f casesen templt = FitsIO $ \s -> getColName' f casesen templt s >>= r2

{# fun unsafe ffgcnn as getColName'
   { withFitsFile*   `FitsFile'
   ,                 `Bool'
   ,                 `String'
   , allocaKeyword-  `String'     peekCString*
   , alloca-         `Int'        peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

getColType     :: FitsFile -> Int -> FitsIO (ColType, Int, Int)
getColType f n = FitsIO $ \s -> getColType' f n s >>= r3

{# fun unsafe ffgtcl as getColType'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , alloca-         `ColType'    peekEnum*
   , alloca-         `Int'        peekIntConv*
   , alloca-         `Int'        peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

getEqColType     :: FitsFile -> Int -> FitsIO (ColType, Int, Int)
getEqColType f n = FitsIO $ \s -> getEqColType' f n s >>= r3

{# fun unsafe ffeqty as getEqColType'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , alloca-         `ColType'    peekEnum*
   , alloca-         `Int'        peekIntConv*
   , alloca-         `Int'        peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Return the display width of a column. This is the length of the
-- string that will be returned by the fits_read_col routine when
-- reading the column as a formatted string. The display width is
-- determined by the TDISPn keyword, if present, otherwise by the data
-- type of the column.

getColDisplayWidth :: FitsFile -> Int -> FitsIO Int
getColDisplayWidth = f2 getColDisplayWidth'

{# fun unsafe ffgcdw as getColDisplayWidth'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   , alloca-         `Int'        peekIntConv*
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Return the number of and size of the dimensions of a table column
-- in a binary table. Normally this information is given by the TDIMn
-- keyword, but if this keyword is not present then this routine
-- returns naxis = 1 and naxes[0] equal to the repeat count in the
-- TFORM keyword.

readTDim           :: FitsFile -> Int -> Int -> FitsIO [Int]
readTDim f col max = FitsIO $ \s ->
  allocaArray max $ \p -> do
    (t, n) <- readTDim' f col max p s
    dim <- peekArray n p
    return (t, map cIntConv dim)

{# fun unsafe ffgtdm as readTDim'
   { withFitsFile*   `FitsFile'
   ,                 `Int'
   ,                 `Int'
   , alloca-         `Int'        peekIntConv*
   , id              `Ptr CLong'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Decode the input TDIMn keyword string (e.g. '(100,200)') and
-- return the number of and size of the dimensions of a binary table
-- column. If the input tdimstr character string is null, then this
-- routine returns naxis = 1 and naxes[0] equal to the repeat count in
-- the TFORM keyword. This routine is called by fits_read_tdim.

decodeTDim             :: FitsFile -> String -> Int -> Int -> FitsIO [Int]
decodeTDim f d col max = FitsIO $ \s ->
  allocaArray max $ \p -> do
    (t, n) <- decodeTDim' f d col max p s
    dim <- peekArray n p
    return (t, map cIntConv dim)

{# fun unsafe ffdtdm as decodeTDim'
   { withFitsFile*   `FitsFile'
   ,                 `String'
   ,                 `Int'
   ,                 `Int'
   , alloca-         `Int'        peekIntConv*
   , id              `Ptr CLong'
   , withStatusConv* `Status'
   } -> `Status' cToStatus
#}

-- | Write a TDIMn keyword whose value has the form '(l,m,n...)' where
-- l, m, n... are the dimensions of a multidimension array column in a
-- binary table.

writeTDim           :: FitsFile -> Int -> [Int] -> FitsIO ()
writeTDim f col dim = FitsIO $ \s ->
  withArrayLen (map cIntConv dim) $ \n p -> do
    t <- writeTDim' f col n p s
    return (t, ())

{# fun unsafe ffptdm as writeTDim'
   { withFitsFile*   `FitsFile'   -- fptr
   ,                 `Int'        -- colnum
   ,                 `Int'        -- naxis
   , id              `Ptr CLong'  -- naxes
   , withStatusConv* `Status'     -- status
   } -> `Status' cToStatus
#}

readColCInt :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCInt fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCInt' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvk as readColCInt'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CInt'                 -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCUInt :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCUInt fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCUInt' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvuk as readColCUInt'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CUInt'                -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCShort :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCShort fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCShort' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvi as readColCShort'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CShort'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCUShort :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCUShort fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCUShort' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvui as readColCUShort'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CUShort'              -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCLong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCLong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCLong' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvj as readColCLong'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CLong'                -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCULong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCULong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCULong' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvuj as readColCULong'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CULong'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCLLong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCLLong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCLLong' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvjj as readColCLLong'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CLLong'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCFloat :: FitsFile -> Int -> Int -> Int -> Int -> Float -> FitsIO ([Float], Int)
readColCFloat fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCFloat' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cFloatConv array, cIntConv anynul))

{# fun unsafe ffgcve as readColCFloat'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Float'                    -- nulval
   , id              `Ptr CFloat'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCDouble :: FitsFile -> Int -> Int -> Int -> Int -> Double -> FitsIO ([Double], Int)
readColCDouble fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCDouble' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cFloatConv array, cIntConv anynul))

{# fun unsafe ffgcvd as readColCDouble'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Double'                   -- nulval
   , id              `Ptr CDouble'              -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCSByte :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCSByte fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCSByte' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvsb as readColCSByte'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CSChar'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColCUByte :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
readColCUByte fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColCUByte' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cIntConv array, cIntConv anynul))

{# fun unsafe ffgcvb as readColCUByte'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Int'                      -- nulval
   , id              `Ptr CUChar'               -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColBool :: FitsFile -> Int -> Int -> Int -> Int -> Bool -> FitsIO ([Bool], Int)
readColBool fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
  allocaArray nelem $ \array' -> do
    (t, anynul) <- readColBool' fptr colnum firstrow firstelem nelem nulval array' s
    array <- peekArray nelem array'
    return (t, (map cToBool array, cIntConv anynul))

{# fun unsafe ffgcvl as readColBool'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `Bool'                     -- nulval
   , id              `Ptr CChar'                -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}

readColString :: FitsFile -> Int -> Int -> Int -> Int -> String -> FitsIO ([String], Int)
readColString fptr colnum firstrow firstelem nelem nulval = do
  len <- getColDisplayWidth fptr colnum
  readColStrLen fptr colnum firstrow firstelem nelem nulval (len+1)

readColStrLen :: FitsFile -> Int -> Int -> Int -> Int -> String -> Int -> FitsIO ([String], Int)
readColStrLen fptr colnum firstrow firstelem nelem nulval len = FitsIO $ \s -> do
  str <- mapM mallocArray (replicate nelem len)
  withArray str $ \str' -> do
    (t, anynul) <- readColStrLen' fptr colnum firstrow firstelem nelem nulval str' s
    array' <- peekArray nelem str'
    array <- mapM peekCString array'
    mapM_ free str
    return (t, (array, cIntConv anynul))

{# fun unsafe ffgcvs as readColStrLen'
   { withFitsFile*   `FitsFile'                 -- fptr
   ,                 `Int'                      -- colnum
   ,                 `Int'                      -- firstrow
   ,                 `Int'                      -- firstelem
   ,                 `Int'                      -- nelem
   ,                 `String'                   -- nulval
   , id              `Ptr (Ptr CChar)'          -- array
   , alloca-         `Int'        peekIntConv*  -- anynul
   , withStatusConv* `Status'                   -- status
   } -> `Status' cToStatus
#}
