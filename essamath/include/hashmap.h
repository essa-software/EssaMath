// hashmap.h
#ifndef HASHMAP_H
#define HASHMAP_H

#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif

// Linked List node
struct EmHashmapNode {
 
    const char* EmKey;
    void* EmValue;
    struct EmHashmapNode* EmNext;
};
 
struct EmHashmap {
 
    size_t EmNumOfElements, EmCapacity;

    struct EmHashmapNode** EmArr;
};

void em_initializehashmap(struct EmHashmap* mp);
void em_hashmapinsert(struct EmHashmap* mp, const char* key, void* value);
void em_deletekey (struct EmHashmap* mp, const char* key);
void em_freehashmap(struct EmHashmap* mp);

void* em_hashmapsearch(struct EmHashmap* mp, const char* key);

#ifdef __cplusplus
}
#endif

#endif // HASHMAP_H
