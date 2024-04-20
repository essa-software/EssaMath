#include "EssaMath.h"
#include <EssaMath/EssaMath.hpp>
#include <unistd.h>

void init_math(){
    int argc = 1;
    char** argv = new char*;
    *argv = new char[256];
    ssize_t count = readlink("/proc/self/exe", *argv, 256);

    Essa::Math::init_math(argc, argv);
}

void free_math(){
    Essa::Math::free_math();
}
