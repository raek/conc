#include <assert.h>
#include <stdlib.h>
#include "runtime.h"

int main(int argc, char *argv[])
{
  Env *env;
  Number *one;
  Number *n;

  env = create_env(NULL);
  assert(env != NULL);
  one = create_number(1);
  assert(one != NULL);
  env_insert(env, "a", (RefCountable*) one);
  n = env_lookup(env, "a");
  assert(n == one);
  free_env(env);

  return 0;
}
