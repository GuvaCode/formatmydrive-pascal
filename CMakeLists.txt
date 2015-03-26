cmake_minimum_required( VERSION 2.8 )

project(FormatMyDrive)

set( APP fmtmydrv)

file(GLOB SOURCES "*.cxx" "*.hpp")

#set( CMAKE_BUILD_TYPE DEBUG)

set( CMAKE_CXX_FLAGS_RELEASE "-DNDEBUG -std=c++11 -Ofast -Wl,-s")

set( CMAKE_CXX_FLAGS_DEBUG "-DDEBUG -std=c++11 -O3 -ggdb")


include (FindPkgConfig)
pkg_search_module(FOX REQUIRED fox17)
pkg_search_module(UDEV REQUIRED libudev)

include_directories(
    ${FOX_INCLUDE_DIRS} 
    ${UDEV_INCLUDE_DIRS} 
    )

link_directories(
    ${FOX_LIBRARY_DIRS} 
    ${UDEV_LIBRARY_DIRS} 
    )

add_executable( 
    ${APP} 
    ${SOURCES} 
    ${FOX_INCLUDE_DIRS} 
    ${UDEV_INCLUDE_DIRS} 
    )

target_link_libraries( 
    ${APP} 
     ${FOX_LIBRARIES} 
     ${UDEV_LIBRARIES} 
    )
