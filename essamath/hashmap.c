#include "hashmap.h"
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
 
    // Default capacity in this case
    mp->EmCapacity = 100;
    mp->EmNumOfElements = 0;
 
    // array of size = 1
    mp->EmArr = (struct EmHashmapNode**)malloc(sizeof(struct EmHashmapNode*) * mp->EmCapacity);
}
 
size_t em_hashfunction(struct EmHashmap* mp, const char* key)
{
    size_t bucketIndex;
    size_t sum = 0, factor = 31;
    for (size_t i = 0; i < strlen(key); i++) {
 
        // sum = sum + (ascii value of
        // char * (primeNumber ^ x))...
        // where x = 1, 2, 3....n
        sum = ((sum % mp->EmCapacity)
               + (((size_t)key[i]) * factor) % mp->EmCapacity)
              % mp->EmCapacity;
 
        // factor = factor * prime
        // number....(prime
        // number) ^ x
        factor = ((factor % __INT16_MAX__)
                  * (31 % __INT16_MAX__))
                 % __INT16_MAX__;
    }
 
    bucketIndex = sum;
    return bucketIndex;
}
 
void em_hashmapinsert(struct EmHashmap* mp, const char* key, void* value)
{
 
    // Getting bucket index for the given
    // key - value pair
    size_t bucketIndex = em_hashfunction(mp, key);
    struct EmHashmapNode* newNode = (struct EmHashmapNode*)malloc(sizeof(struct EmHashmapNode));
 
    // Setting value of node
    em_setnode(newNode, key, value);
 
    // Bucket index is empty....no collision
    if (mp->EmArr[bucketIndex] == NULL) {
        mp->EmArr[bucketIndex] = newNode;
    }
 
    // Collision
    else {
 
        // Adding newNode at the head of
        // linked list which is present
        // at bucket index....insertion at
        // head in linked list
        newNode->EmNext = mp->EmArr[bucketIndex];
        mp->EmArr[bucketIndex] = newNode;
    }
}
 
void em_deletekey (struct EmHashmap* mp, const char* key)
{
 
    // Getting bucket index for the
    // given key
    size_t bucketIndex = em_hashfunction(mp, key);
 
    struct EmHashmapNode* prevNode = NULL;
 
    // Points to the head of
    // linked list present at
    // bucket index
    struct EmHashmapNode* currNode = mp->EmArr[bucketIndex];
 
    while (currNode != NULL) {
 
        // Key is matched at delete this
        // node from linked list
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
 
    // Getting the bucket index
    // for the given key
    size_t bucketIndex = em_hashfunction(mp, key);
 
    // Head of the linked list
    // present at bucket index
    struct EmHashmapNode* bucketHead = mp->EmArr[bucketIndex];
    while (bucketHead != NULL) {
 
        // Key is found in the hashMap
        if (bucketHead->EmKey == key) {
            return bucketHead->EmValue;
        }
        bucketHead = bucketHead->EmNext;
    }
 
    // If no key found in the hashMap
    // equal to the given key
    char* errorMssg = (char*)malloc(sizeof(char) * 25);
    errorMssg = "Oops! No data found.\n";
    return errorMssg;
}
