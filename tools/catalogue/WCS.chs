module WCS where

#include "wcs/wcs.h"

import C2HS

{# fun pure unsafe fk425e
   { withFloatConv* `Float' peekFloatConv*
   , withFloatConv* `Float' peekFloatConv*
   , cFloatConv     `Float'
   } -> `()'
#}

{# fun pure unsafe fk524e
   { withFloatConv* `Float' peekFloatConv*
   , withFloatConv* `Float' peekFloatConv*
   , cFloatConv     `Float'
   } -> `()'
#}
