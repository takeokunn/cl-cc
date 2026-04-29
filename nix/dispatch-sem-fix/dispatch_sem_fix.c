#include <mach/mach.h>
#include <mach/semaphore.h>
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

typedef void *dispatch_semaphore_t;
typedef void *dispatch_object_t;
typedef uint64_t dispatch_time_t;

#ifndef DISPATCH_TIME_FOREVER
#define DISPATCH_TIME_FOREVER (~(uint64_t)0)
#endif

typedef struct {
  uint64_t    magic;
  semaphore_t mach_sem;
} our_sem_t;

#define OUR_MAGIC 0xDEADBEEFCAFEBABEULL

static int is_ours(void *d) {
  if (!d) return 0;
  return ((our_sem_t*)d)->magic == OUR_MAGIC;
}

dispatch_semaphore_t dispatch_semaphore_create(long value) {
  our_sem_t *s = (our_sem_t*)calloc(1, sizeof(our_sem_t));
  if (!s) return NULL;
  kern_return_t kr = semaphore_create(mach_task_self(), &s->mach_sem,
                                      SYNC_POLICY_FIFO, (int)value);
  if (kr != KERN_SUCCESS) { free(s); return NULL; }
  s->magic = OUR_MAGIC;
  return (dispatch_semaphore_t)s;
}

long dispatch_semaphore_signal(dispatch_semaphore_t d) {
  if (!is_ours(d)) return 0;
  our_sem_t *s = (our_sem_t*)d;
  semaphore_signal(s->mach_sem);
  return 0;
}

long dispatch_semaphore_wait(dispatch_semaphore_t d, dispatch_time_t timeout) {
  if (!is_ours(d)) return 0;
  our_sem_t *s = (our_sem_t*)d;
  kern_return_t kr;
  if (timeout == DISPATCH_TIME_FOREVER) {
    do { kr = semaphore_wait(s->mach_sem); } while (kr == KERN_ABORTED);
  } else {
    mach_timespec_t ts = {
      .tv_sec  = (unsigned int)((uint64_t)timeout / 1000000000ULL),
      .tv_nsec = (clock_res_t)((uint64_t)timeout % 1000000000ULL)
    };
    do { kr = semaphore_timedwait(s->mach_sem, ts); } while (kr == KERN_ABORTED);
  }
  return (kr == KERN_SUCCESS) ? 0 : 1;
}

void dispatch_release(dispatch_object_t object) {
  if (is_ours(object)) {
    our_sem_t *s = (our_sem_t*)object;
    s->magic = 0;
    semaphore_destroy(mach_task_self(), s->mach_sem);
    free(s);
    return;
  }
  static void (*real)(dispatch_object_t);
  if (!real) real = dlsym(RTLD_NEXT, "dispatch_release");
  if (real) real(object);
}
