#include "mongoose.h"

const char *getstr(struct mg_str s)
{
  return s.p;
}
