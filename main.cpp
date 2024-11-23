#include <iostream>
#include "repl/repl.h"

int main() {
    std::cout << "Welcome to Monkey++!" << std::endl;
    repl::Start(std::cin, std::cout);
    return 0;
}
