cmake_minimum_required(VERSION 3.30)
project(MonkeyCpp)

set(CMAKE_OSX_ARCHITECTURES "arm64")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUILD_TYPE Debug)

# Include directories
include_directories(${PROJECT_SOURCE_DIR})

# Fetch GoogleTest
include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)

# For Windows: Prevent overriding the parent project's compiler/linker settings
if(WIN32)
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()

FetchContent_MakeAvailable(googletest)

# Add the lexer library
add_library(lexer_lib lexer/lexer.cpp token/token.cpp)
# Link the lexer library with the token header
target_include_directories(lexer_lib PUBLIC ${PROJECT_SOURCE_DIR}/token)
# Add the test executable for lexer
add_executable(lexer_test lexer/lexer_test.cpp)
# Link the lexer test executable with the lexer library and GoogleTest
target_link_libraries(lexer_test lexer_lib gtest gtest_main)
# Include GoogleTest headers
target_include_directories(lexer_test PRIVATE ${gtest_SOURCE_DIR}/include)

# Add the executable for the REPL (start program)
add_executable(start main.cpp repl/repl.cpp)
# Link the start executable with the lexer library and other dependencies
target_link_libraries(start lexer_lib)
# Include directories for the REPL
target_include_directories(start PUBLIC ${PROJECT_SOURCE_DIR}/repl)
