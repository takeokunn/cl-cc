#include <mach/mach.h>
#include <mach/semaphore.h>
#include <mach/mach_time.h>
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
  } else if (timeout == 0) {
    /* DISPATCH_TIME_NOW: non-blocking try.  A single attempt is correct here:
       retrying on KERN_ABORTED would spin if signals arrive continuously, which
       defeats the non-blocking contract.  A spurious KERN_ABORTED is treated as
       "not acquired" — acceptable for a try-acquire. */
    mach_timespec_t ts = { .tv_sec = 0, .tv_nsec = 0 };
    kr = semaphore_timedwait(s->mach_sem, ts);
    return (kr == KERN_SUCCESS) ? 0 : 1;
  } else {
    /* timeout is an absolute mach-tick value matching mach_absolute_time().
       Convert the remaining ticks to nanoseconds via mach_timebase_info so that
       the relative duration passed to semaphore_timedwait is correct regardless
       of the hardware tick rate (do not assume 1 tick == 1 ns). */
    uint64_t now_ticks = mach_absolute_time();
    if (now_ticks >= timeout) return 1;
    mach_timebase_info_data_t tb;
    if (mach_timebase_info(&tb) != KERN_SUCCESS || tb.denom == 0) return 1;
    uint64_t delta = timeout - now_ticks;
    uint64_t rel_ns = (tb.numer == 0 || delta > UINT64_MAX / tb.numer)
                      ? UINT64_MAX
                      : delta * tb.numer / tb.denom;
    mach_timespec_t ts = {
      .tv_sec  = (unsigned int)(rel_ns / 1000000000ULL),
      .tv_nsec = (clock_res_t)(rel_ns % 1000000000ULL)
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
