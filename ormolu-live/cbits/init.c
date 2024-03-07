#include "Rts.h"

#include "Main_stub.h"

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  evaluateFixityInfo();
  hs_perform_gc();
  hs_perform_gc();
  rts_clearMemory();
}
