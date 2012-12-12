#include <stdint.h>
#include <stdlib.h> // malloc
#include "runtime.h"

void init_ref_count(RefCount *ref_count, FnFree free_fn)
{
  ref_count->count = 0;
  ref_count->free_fn = free_fn;
}

void ref_count_acquire(RefCountable *ref_countable)
{
  RefCount *ref_count;

  ref_count = &ref_countable->ref_count;
  ref_count->count += 1;
}

void ref_count_release(RefCountable *ref_countable)
{
  RefCount *ref_count;

  ref_count = &ref_countable->ref_count;
  ref_count->count -= 1;
  if (ref_count->count == 0)
    {
      ref_count->free_fn(ref_countable);
    }
}

Env *create_env(Env *parent)
{
  Env *result;

  result = (Env*) malloc(sizeof(Env));
  init_ref_count(&result->ref_count, free_env);
  result->count = 0;
  result->bindings = NULL;
  result->parent = parent;
  return result;
}

void free_env(void *ptr)
{
  Env *env;

  env = (Env*) ptr;
  if (env->count != 0)
    {
      uint16_t i;

      for (i = 0; i < env->count; i += 1)
        {
          ref_count_release(env->bindings[i].value);
        }
      free(env->bindings);
    }
  free(env);
}

void env_insert(Env *env, char *key, RefCountable *ref_countable)
{
  Pair p;

  env->count += 1;
  env->bindings = realloc(env->bindings, env->count * sizeof(*env->bindings));
  p.name = key;
  p.value = ref_countable;
  env->bindings[env->count - 1] = p;
  ref_count_acquire(ref_countable);
}

void *env_lookup(Env *env, char *key)
{
  Env *currentEnv;
  int i;

  currentEnv = env;
  do
    {
      for (i = 0; i < currentEnv->count; i += 1)
        {
          if (strcmp(currentEnv->bindings[i].name, key) == 0)
            {
              return currentEnv->bindings[i].value;
            }
        }
      currentEnv = currentEnv->parent;
    }
  while (currentEnv != NULL);

  return NULL;
}

Number *create_number(int i)
{
  Number *n;

  n = (Number*) malloc(sizeof(Number));
  init_ref_count(&n->ref_count, free_number);
  n->value = i;
  return n;
}

void free_number(void *number)
{
  free(number);
}
