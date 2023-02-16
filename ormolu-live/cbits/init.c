#include "Main_stub.h"
#include <Rts.h>

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *args[] = {
      "ormolu-live.wasm", "+RTS", "--nonmoving-gc", "-H64m", "-RTS", NULL};
  int argc = sizeof(args) / sizeof(args[0]) - 1;
  char **argv = args;
  hs_init_with_rtsopts(&argc, &argv);
  evaluateFixityInfo();
  hs_perform_gc();
  hs_perform_gc();
  rts_clearMemory();
}
