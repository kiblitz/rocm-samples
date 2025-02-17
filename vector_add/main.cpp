#include <iostream>
#include <dlfcn.h>

int main() {
    void* handle = dlopen("./vectoradd_hip.so", RTLD_LAZY);
    if (!handle) {
        std::cerr << "Failed to load lib: " << dlerror() << std::endl;
        return 1;
    }

    dlerror();

    typedef int (*F)();
    F f = (F)dlsym(handle, "f");

    char* error = dlerror();
    if (error) {
        std::cerr << "Failed to load symbol: " << error << std::endl;
        dlclose(handle);
        return 1;
    }

    int i = f();
    std::cout << "Errors: " << i << std::endl;

    dlclose(handle);
    return 0;
}