project(Satisfy)
cmake_minimum_required(VERSION 3.20.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

find_package(LLVM REQUIRED)

if(NOT LLVM_FOUND)
  message(FATAL "Failed to find llvm.\nSet LLVM_ROOT_DIR")
else()
  message(${LLVM_ROOT_DIR})
endif()

if(CMAKE_BUILD_TYPE MATCHES Debug)
  add_compile_options(-g)
  message("Added debug options")
endif()

file(GLOB SOURCE_FILES
  *.cpp
  )

add_executable(${PROJECT_NAME} ${SOURCE_FILES})
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_SOURCE_DIR})
target_include_directories(${PROJECT_NAME} PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PUBLIC ${LLVM_LIBRARIES})
target_link_directories(${PROJECT_NAME} PUBLIC ${LLVM_LIBRARY_DIRS})
target_compile_options(${PROJECT_NAME} PUBLIC ${LLVM_CXX_FLAGS})
target_link_options(${PROJECT_NAME} PUBLIC ${LLVM_LDFLAGS})
