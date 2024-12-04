#include <iostream>
#include "repl/repl.h"

int main() {
    std::cout << "Welcome to MonkeyCxx!" << std::endl;
    repl::Start(std::cin, std::cout);
    return 0;
}
