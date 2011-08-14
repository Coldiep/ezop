
cmake_minimum_required(VERSION 2.6)

project(Ezop CXX)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)

# Общие флаги для всех сборок.
add_definitions(-pipe -fPIC -Wall)

if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
#   Добавление профилировки: для включения профилировки запускаем cmake -DGPROF=1.
    if(GPROF)
        add_definitions(-pg)
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
    endif(GPROF)

#   Добавляем флаги для построения отладочной инфомрации.
    add_definitions(-fno-inline-functions -fno-inline -ggdb3)
elseif(${CMAKE_BUILD_TYPE} STREQUAL "Release")
    add_definitions(-O2)
endif(${CMAKE_BUILD_TYPE} STREQUAL "Debug")

# Устанавливаем зависимости на boost.
set(Boost_USE_MULTITHREADED ON)

find_package(Boost 1.41.0 COMPONENTS
    date_time
    filesystem
    program_options
    system
)
if(Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIRS})
else()
  message(FATAL_ERROR "Boost libraries not found. Please get a copy from http://www.boost.org")
endif()

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${PROJECT_SOURCE_DIR}/parser
    ${PROJECT_SOURCE_DIR}/terms
    ${PROJECT_SOURCE_DIR}/lexers/re-lexer
)

add_subdirectory(parser)
add_subdirectory(terms)
add_subdirectory(lexers/re-lexer)
add_subdirectory(onto)
add_subdirectory(web)
add_subdirectory(prolog/sandbox)

set(NAME ezop)

add_executable(${NAME}
    main.cpp
)

target_link_libraries (${NAME}
          parser
          terms
          re-lexer
)
