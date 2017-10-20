/* Emacs module for SCWS.

Copyright (C) 2017 by Chunyang Xu

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <emacs-module.h>

#include <scws/scws.h>

int plugin_is_GPL_compatible;

static char *
retrieve_string (emacs_env *env, emacs_value str)
{
  char *buf = NULL;
  ptrdiff_t size = 0;

  env->copy_string_contents (env, str, NULL, &size);

  buf = malloc (size);
  if (buf == NULL) return NULL;

  env->copy_string_contents (env, str, buf, &size);

  return buf;
}

static void
scws_module_free (void *arg)
{
  scws_free ((scws_t) arg);
}

static emacs_value
Fscws_module_new (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  char *dict;
  char *rule;
  scws_t s;

  dict = retrieve_string (env, args[0]);
  rule = retrieve_string (env, args[1]);
  s = scws_new ();
  if (s == NULL) return env->intern (env, "nil");
  scws_set_charset (s, "utf8");
  scws_set_dict (s, dict, SCWS_XDICT_XDB);
  scws_set_rule (s, rule);

  return env->make_user_ptr (env, scws_module_free, s);
}

static emacs_value
Fscws_module_send_text (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  scws_t s;
  char *text;
  scws_res_t res, cur;
  emacs_value values = env->intern (env, "nil");
  emacs_value Qcons = env->intern (env, "cons");

  s = env->get_user_ptr (env, args[0]);
  text = retrieve_string (env, args[1]);
  scws_send_text (s, text, strlen (text));
  while ((res = cur = scws_get_result (s)))
    {
      while (cur != NULL)
        {
          emacs_value cargs[] =
            { env->make_string (env, text + cur->off, cur->len),
              values };
          values = env->funcall (env, Qcons, 2, cargs);
          cur = cur->next;
        }
      scws_free_result (res);
    }
  emacs_value rargs[] = {values};
  return env->funcall (env, env->intern (env, "reverse"), 1, rargs);
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  if (ert->size < sizeof *ert)
    {
      fprintf (stderr, "Runtime size of runtime structure (%td bytes) "
               "smaller than compile-time size (%zu bytes)",
               ert->size, sizeof *ert);
      return 1;
    }

  emacs_env *env = ert->get_environment (ert);

  if (env->size < sizeof *env)
    {
      fprintf (stderr, "Runtime size of environment structure (%td bytes) "
               "smaller than compile-time size (%zu bytes)",
               env->size, sizeof *env);
      return 2;
    }

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("scws-module-new", Fscws_module_new, 2, 2, "初始化 scws 对象，参数为词典和规则文件.", NULL);
  DEFUN ("scws-module-send-text", Fscws_module_send_text, 2, 2, "发送待分词的字符串，参数为 scws 对象和字符串.", NULL);

#undef DEFUN

  provide (env, "scws-module");
  return 0;
}
