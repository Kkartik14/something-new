# CMakeLists.txt — Build configuration for Adam JNI bridge.
#
# This is used by the Android Gradle plugin to compile the JNI C code
# and link it with the pre-built Adam runtime.

cmake_minimum_required(VERSION 3.22.1)
project(adam_jni C)

# The JNI bridge source.
add_library(adam_jni SHARED
    adam_jni.c
    apis.c
)

# Link with Android system libraries.
target_link_libraries(adam_jni
    android
    log
    EGL
    GLESv3
)

# Link with the pre-built Adam runtime static library.
# The library is expected to be at: app/src/main/jniLibs/<abi>/libadam_runtime.a
set(ADAM_RUNTIME_DIR ${CMAKE_SOURCE_DIR}/../app/src/main/jniLibs/${ANDROID_ABI})

if(EXISTS "${ADAM_RUNTIME_DIR}/libadam_runtime.a")
    add_library(adam_runtime STATIC IMPORTED)
    set_target_properties(adam_runtime PROPERTIES
        IMPORTED_LOCATION "${ADAM_RUNTIME_DIR}/libadam_runtime.a"
    )
    target_link_libraries(adam_jni adam_runtime)
else()
    message(WARNING "Adam runtime library not found at ${ADAM_RUNTIME_DIR}/libadam_runtime.a")
endif()

# Link with Skia if available.
if(EXISTS "${ADAM_RUNTIME_DIR}/libskia.a")
    add_library(skia STATIC IMPORTED)
    set_target_properties(skia PROPERTIES
        IMPORTED_LOCATION "${ADAM_RUNTIME_DIR}/libskia.a"
    )
    target_link_libraries(adam_jni skia)
endif()
