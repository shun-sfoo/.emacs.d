#define NOB_IMPLEMENTATION
#include "nob.h"

#define BUILD_FOLDER "build/"
#define SRC_FOLDER   "src/"

int main(int argc, char** argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);
  
    if (!nob_mkdir_if_not_exists(BUILD_FOLDER)) return 1;

    Nob_Cmd cmd = {0};

    nob_cmd_append(&cmd, "cc", "-Wall", "-Wextra", "-o", BUILD_FOLDER"hello", SRC_FOLDER"hello.c");

    if (!nob_cmd_run(&cmd)) return 1;
    
}
