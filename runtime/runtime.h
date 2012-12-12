#include <stdint.h>

struct Call;
struct Env;
struct Args;

typedef void(*FnFree)(void*);
typedef struct Call *(*FnSpec)(struct Env*, struct Args*);

typedef struct {
  uint16_t count;
  FnFree free_fn;
} RefCount;

typedef struct {
  RefCount ref_count;
} RefCountable;

typedef struct {
  char *name;
  RefCountable *value;
} Pair;

typedef struct _Env {
  RefCount ref_count;
  uint16_t count;
  Pair *bindings;
  struct _Env *parent;
} Env;

typedef struct {
  RefCount ref_count;
  uint16_t count;
  RefCountable **args;
} Args;

typedef struct {
  RefCount ref_count;
  int16_t value;
} Number;

typedef struct {
  RefCount ref_count;
  FnSpec fn_spec;
  Env *env;
} Closure;

typedef struct {
  RefCount ref_count;
  Closure *closure;
  Args *args;
} Call;

void init_ref_count(RefCount *ref_count, FnFree free_fn);
void ref_count_acquire(RefCountable *ref_count);
void ref_count_release(RefCountable *ref_count);

Env *create_env(Env *parent);
void env_insert(Env *env, char *key, RefCountable *ref_countable);
void *env_lookup(Env *env, char *key);
void free_env(void *env);

Args *create_args(int size);
void args_set(Args *args, int i, RefCountable *ref_countable);
void *args_get(Args *args, int i);
void free_args(void *args);

Number *create_number(int i);
void free_number(void *number);

Closure *create_closure(FnSpec fn_spec, Env env);
void free_closure(void *closure);
