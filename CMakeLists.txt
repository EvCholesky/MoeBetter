cmake_minimum_required(VERSION 3.8.0)
project(moeb)
file(GLOB SOURCE "source/*.cpp")
file(GLOB HEADERS "source/*.h" "source/*.inl")
file(GLOB UTIL "moeb.natvis")
add_executable(moeb ${SOURCE} ${HEADERS} ${UTIL})

if (MSVC)

else()
	target_compile_options(moeb PRIVATE -Wno-switch)
endif()

# Tests
