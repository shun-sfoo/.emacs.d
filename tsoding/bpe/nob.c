#define NOB_IMPLEMENTATION
#include "nob.h"

#define BUILD_FOLDER "build/"

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc , argv);
    
    Cmd cmd = {0};
    if(!nob_mkdir_if_not_exists(BUILD_FOLDER)) return 1;
    cmd_append(&cmd, "cc" , "-Wall", "-Wextra","-o", BUILD_FOLDER"bpe", "bpe.c");
     if(!nob_cmd_run(&cmd)) return 1;
    return 0;
}
