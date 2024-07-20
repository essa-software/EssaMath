#include "hashmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// like constructor
void em_setnode(struct EmHashmapNode* node, const char* key, void* value)
{
    node->EmKey = key;
    node->EmValue = value;
    node->EmNext = NULL;
}
 
// like constructor
void em_initializehashmap(struct EmHashmap* mp)
{
    mp->EmCapacity = 100;
    mp->EmNumOfElements = 0;
 
    mp->EmArr = (struct EmHashmapNode**)malloc(sizeof(struct EmHashmapNode*) * mp->EmCapacity);
}
 
size_t em_hashfunction(struct EmHashmap* mp, const char* key)
{
    size_t bucketIndex;
    size_t sum = 0, factor = 31;
    for (size_t i = 0; i < strlen(key); i++) {
        sum = ((sum % mp->EmCapacity)
               + (((size_t)key[i]) * factor) % mp->EmCapacity)
              % mp->EmCapacity;
        factor = ((factor % __INT16_MAX__)
                  * (31 % __INT16_MAX__))
                 % __INT16_MAX__;
    }
 
    bucketIndex = sum;
    return bucketIndex;
}
 
void em_hashmapinsert(struct EmHashmap* mp, const char* key, void* value)
{
 
    size_t bucketIndex = em_hashfunction(mp, key);
    struct EmHashmapNode* newNode = (struct EmHashmapNode*)malloc(sizeof(struct EmHashmapNode));
 
    // Setting value of node
    em_setnode(newNode, key, value);
 
    // Bucket index is empty....no collision
    if (mp->EmArr[bucketIndex] == NULL) {
        mp->EmArr[bucketIndex] = newNode;
    }
    else {
        newNode->EmNext = mp->EmArr[bucketIndex];
        mp->EmArr[bucketIndex] = newNode;
    }
}
 
void em_deletekey (struct EmHashmap* mp, const char* key)
{
    size_t bucketIndex = em_hashfunction(mp, key);
 
    struct EmHashmapNode* prevNode = NULL;
    struct EmHashmapNode* currNode = mp->EmArr[bucketIndex];
 
    while (currNode != NULL) {
        if (strcmp(key, currNode->EmKey) == 0) {
 
            // Head node
            // deletion
            if (currNode == mp->EmArr[bucketIndex]) {
                mp->EmArr[bucketIndex] = currNode->EmNext;
            }
 
            // Last node or middle node
            else {
                prevNode->EmNext = currNode->EmNext;
            }
            free(currNode);
            break;
        }
        prevNode = currNode;
        currNode = currNode->EmNext;
    }
}

void em_freehashmap(struct EmHashmap* mp){
    for(size_t i = 0; i < mp->EmCapacity; i++){
        
        struct EmHashmapNode* prevNode = NULL;
        struct EmHashmapNode* currNode = mp->EmArr[i];
    
        while (currNode != NULL) {
    
            prevNode = currNode;
            currNode = currNode->EmNext;

            if(prevNode){
                free(prevNode);
            }
        }
    }
    
    free(mp->EmArr);
}
 
void* em_hashmapsearch(struct EmHashmap* mp, const char* key)
{
    size_t bucketIndex = em_hashfunction(mp, key);
 
    struct EmHashmapNode* bucketHead = mp->EmArr[bucketIndex];
    while (bucketHead != NULL) {
 
        if (strcmp(bucketHead->EmKey, key) == 0) {
            return bucketHead->EmValue;
        }
        bucketHead = bucketHead->EmNext;
    }
 
    return NULL;
}
