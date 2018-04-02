cmake_minimum_required(VERSION 2.8.9)

# General settings
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS " -g -std=c++11 -std=gnu++11")

set(PARSER_DIR ${CMAKE_SOURCE_DIR}/parser)
set(SYNTAX_TREE_DIR ${CMAKE_SOURCE_DIR}/syntax_tree)
set(UTILS_DIR ${CMAKE_SOURCE_DIR}/utils)
set(CODEGEN_DIR ${CMAKE_SOURCE_DIR}/code_gen)

# Flex and Bison
find_package(BISON 3.0)
find_package(FLEX)

BISON_TARGET(parser ${PARSER_DIR}/c.y ${PARSER_DIR}/parser.cpp)
FLEX_TARGET(lexer ${PARSER_DIR}/c.l ${PARSER_DIR}/lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(lexer parser)

# LLVM
set(LLVM_DIR /usr/local/share/llvm/cmake)
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
add_definitions(${LLVM_DEFINITIONS})
set(LLVM_LINK_COMPONENTS
  Analysis
  BitReader
  BitWriter
  Core
  ExecutionEngine
  InstCombine
  Interpreter
  MC
  MCDisassembler
  MCJIT
  Object
  RuntimeDyld
  ScalarOpts
  Support
  Target
  TransformUtils
  native 
  nativecodegen
)
llvm_map_components_to_libraries(llvm_libs ${LLVM_LINK_COMPONENTS})
message(STATUS "LLVM_LIBS : ${llvm_libs}")

# Other sources
file(GLOB SYNTAX_TREE_SOURCES "${SYNTAX_TREE_DIR}/*.cpp")
file(GLOB UTILS_SOURCES "${UTILS_DIR}/*.cpp") 
file(GLOB CODEGEN_SOURCES "${CODEGEN_DIR}/*.cpp") 

# Includes
include_directories(
					${SYNTAX_TREE_DIR}
					${UTILS_DIR}
					${PARSER_DIR}
					${CODEGEN_DIR}
					${LLVM_INCLUDE_DIRS}
					/usr/include/llvm-c-3.4
					/usr/include/llvm-3.4
				)

# Executable
add_executable(
				cc
				${CMAKE_SOURCE_DIR}/cc.cpp
				${BISON_parser_OUTPUTS}
				${FLEX_lexer_OUTPUTS}
				${SYNTAX_TREE_SOURCES}
				${UTILS_SOURCES}
				${CODEGEN_SOURCES}
			)

# Link executable to LLVM static libraries
target_link_libraries(
						cc
						${llvm_libs}
					)
