#include <scws/scws.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DICT "/usr/local/etc/dict.utf8.xdb"
#define RULE "/usr/local/etc/rules.utf8.ini"

int
main (void)
{
  scws_t s;
  scws_res_t res, cur;
  char *text = "我来到北京清华大学";

  if ((s = scws_new ()) == NULL)
    {
      printf ("ERROR: Can't init the scws!\n");
      exit (-1);
    }
  scws_set_charset (s, "utf8");
  scws_set_dict (s, DICT, SCWS_XDICT_XDB);
  scws_set_rule (s, RULE);

  puts (text);
  scws_send_text (s, text, strlen (text));
  while ((res = cur = scws_get_result (s)))
    {
      while (cur != NULL)
        {
          printf ("WORD: %.*s/%s (IDF = %4.2f)\n",
                  cur->len,
                  text+cur->off,
                  cur->attr,
                  cur->idf);
          cur = cur->next;
        }
      scws_free_result (res);
    }
  scws_free (s);
  return 0;
}
