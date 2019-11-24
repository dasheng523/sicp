#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mongoose.h"

const char *getstr(struct mg_str s)
{
  return s.p;
}

const char *getstr2(struct mg_str *s)
{
  return s->p;
}

size_t getLen(struct mg_str s)
{
  return s.len;
}

size_t getLen2(struct mg_str *s)
{
  return s->len;
}

struct mg_str getMessage(struct http_message *hm)
{
  return hm->message;
}

struct mg_str getBody(struct http_message *hm)
{
  return hm->body;
}

struct mg_str getQueryString(struct http_message *hm)
{
  return hm->query_string;
}

struct mg_str *getHeaderNames(struct http_message *hm)
{
  return hm->header_names;
}

struct mg_str *getHeaderValues(struct http_message *hm)
{
  return hm->header_values;
}
