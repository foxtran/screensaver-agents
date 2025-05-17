cmake_minimum_required(VERSION 3.22)

project(screensaver
  VERSION 0.0.1
  LANGUAGES Fortran)

find_package(OpenMP REQUIRED)

include(CheckIPOSupported)
check_ipo_supported(RESULT LTO_AVAIL)

set(SOURCE
  src/main.f90
  src/const.f90
  src/posix.f90
  src/utils.f90
  src/field.f90
  src/agent.f90
)

add_executable(${PROJECT_NAME} ${SOURCE})
target_link_libraries(${PROJECT_NAME} OpenMP::OpenMP_Fortran)
if(LTO_AVAIL)
  set_property(TARGET ${PROJECT_NAME} PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
