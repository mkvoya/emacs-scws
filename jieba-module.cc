/** Emacs module for jieba.
 *
 *  Copyright (C) 2017 by Chunyang Xu
 *  Copyright (C) 2021 by Mingkai Dong
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
**/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <emacs-module.h>

#include "cppjieba/include/cppjieba/Jieba.hpp"

int plugin_is_GPL_compatible;

static std::string
retrieve_string (emacs_env *env, emacs_value str)
{
	ptrdiff_t size = 0;

	env->copy_string_contents (env, str, NULL, &size);

	std::string s;
	s.reserve(size);

	env->copy_string_contents (env, str, &s.front(), &size);

	return s;
}

static void
jieba_module_free (void *arg) noexcept
{
	delete static_cast<cppjieba::Jieba *>(arg);
}


const char* const DICT_PATH = "cppjieba/dict/jieba.dict.utf8";
const char* const HMM_PATH = "cppjieba/dict/hmm_model.utf8";
const char* const USER_DICT_PATH = "cppjieba/dict/user.dict.utf8";
const char* const IDF_PATH = "cppjieba/dict/idf.utf8";
const char* const STOP_WORD_PATH = "cppjieba/dict/stop_words.utf8";

static emacs_value
Fjieba_module_new (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) noexcept
{
	cppjieba::Jieba *jieba;
	std::string dict;
	std::string rule;

	dict = retrieve_string (env, args[0]);
	rule = retrieve_string (env, args[1]);
	jieba = new cppjieba::Jieba(DICT_PATH,
				    HMM_PATH,
				    USER_DICT_PATH,
				    IDF_PATH,
				    STOP_WORD_PATH);
	if (jieba == NULL) return env->intern (env, "nil");

	return env->make_user_ptr (env, jieba_module_free, jieba);
}

static emacs_value
Fjieba_module_send_text (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) noexcept
{
	cppjieba::Jieba *jieba;

	std::string text;
	std::vector<std::string> words;
	emacs_value values = env->intern (env, "nil");
	emacs_value Qcons = env->intern (env, "cons");

	jieba = static_cast<cppjieba::Jieba *>(env->get_user_ptr (env, args[0]));
	std::cout << text << std::endl;
	text = retrieve_string (env, args[1]);
	jieba->Cut(text, words);
	for (const auto &word: words) {
		emacs_value cargs[] =
		{ env->make_string (env, word.c_str(), word.size()),
			values };
		values = env->funcall (env, Qcons, 2, cargs);
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
emacs_module_init (struct emacs_runtime *ert) noexcept
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

	DEFUN ("jieba-module-new", Fjieba_module_new, 2, 2, "初始化 jieba 对象，参数为词典和规则文件.", NULL);
	DEFUN ("jieba-module-send-text", Fjieba_module_send_text, 2, 2, "发送待分词的字符串，参数为 jieba 对象和字符串.", NULL);

#undef DEFUN

	provide (env, "jieba-module");
	return 0;
}
