#ifndef CL_CC_H
#define CL_CC_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct cl_cc_state cl_cc_state_t;

typedef enum cl_cc_value_kind {
  CL_CC_VALUE_NIL = 0,
  CL_CC_VALUE_INTEGER = 1,
  CL_CC_VALUE_FLOAT = 2,
  CL_CC_VALUE_STRING = 3,
  CL_CC_VALUE_SYMBOL = 4,
  CL_CC_VALUE_FUNCTION = 5,
  CL_CC_VALUE_OBJECT = 6,
  CL_CC_VALUE_ERROR = 7
} cl_cc_value_kind_t;

typedef struct cl_cc_value {
  cl_cc_value_kind_t kind;
  union {
    int64_t integer;
    double floating;
    const char *string;
    void *object;
  } as;
} cl_cc_value_t;

typedef struct cl_cc_error {
  int code;
  const char *message;
} cl_cc_error_t;

typedef cl_cc_value_t (*cl_cc_callback_t)(cl_cc_state_t *state,
                                          size_t argc,
                                          const cl_cc_value_t *argv,
                                          void *userdata);

cl_cc_state_t *cl_cc_init(void);
cl_cc_value_t cl_cc_eval(cl_cc_state_t *state, const char *code);
cl_cc_value_t cl_cc_call(cl_cc_state_t *state, const char *fn, ...);
void cl_cc_cleanup(cl_cc_state_t *state);
cl_cc_error_t cl_cc_last_error(cl_cc_state_t *state);

int cl_cc_register_callback(cl_cc_state_t *state,
                            const char *name,
                            cl_cc_callback_t callback,
                            void *userdata);
void *cl_cc_get_callback(cl_cc_state_t *state, const char *name);

#ifdef __cplusplus
}
#endif

#endif /* CL_CC_H */
