#include "CloseFile.h"
#include "longnam.h"

void closeFile(fitsfile *fptr)
{
  int status = 0; (void) fits_close_file(fptr, &status);
}
