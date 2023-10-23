#include "Rts.h"

#include "Main_stub.h"

void malloc_inspect_all(void (*handler)(void *start, void *end,
                                        size_t used_bytes, void *callback_arg),
                        void *arg);

static void malloc_inspect_all_handler(void *start, void *end,
                                       size_t used_bytes, void *callback_arg) {
  if (used_bytes == 0) {
    memset(start, 0, (size_t)end - (size_t)start);
  }
}

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *args[] = {
      "ormolu-live.wasm", "+RTS", "-H64m", "-RTS", NULL};
  int argc = sizeof(args) / sizeof(args[0]) - 1;
  char **argv = args;
  hs_init_with_rtsopts(&argc, &argv);
  evaluateFixityInfo();
  hs_perform_gc();
  hs_perform_gc();
  rts_clearMemory();
  malloc_inspect_all(malloc_inspect_all_handler, NULL);
  memset(0, 0, (size_t)__builtin_frame_address(0));
}
