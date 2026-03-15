#include <stdio.h>
#include <string.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

typedef struct {
    char key[2];
    size_t value;
} KV;

int main(int argc, char **argv) {
    const char * text = "The original BPE algorithm operates by iteratively replacing the most common contiguous sequences of characters in a target text with unused 'placeholder' bytes. The iteration ends when no sequences can be found, leaving the target text effectively compressed. Decompression can be performed by reversing this process, querying known placeholder terms against their corresponding denoted sequence, using a lookup table. In the original paper, this lookup table is encoded and stored alongside the compressed text.";

    KV *frep;
    
    int text_size = strlen(text);
    for (int i = 0; i < text_size - 1; ++i) {
        char key[2] = {text[i] , text[i+1]};
        ptrdiff
        
        
    }
    
    return 0;
}
