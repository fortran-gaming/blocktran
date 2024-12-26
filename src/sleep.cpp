#include <chrono>
#include <thread>

#include "csleep.h"

void c_sleep(int* milliseconds){
    std::this_thread::sleep_for(std::chrono::milliseconds(*milliseconds));
}
