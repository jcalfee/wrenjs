extern "C" {
#include "../wren/src/include/wren.h"
}
#include <emscripten/bind.h>
using namespace emscripten;

int lerp() {
    return WREN_VERSION_NUMBER;
}

EMSCRIPTEN_BINDINGS(my_module) {
    function("lerp", &lerp);
}