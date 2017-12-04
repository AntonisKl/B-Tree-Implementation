#include "AM.h"

#include "bf.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <stdbool.h>

#define MAX_OPEN_FILES 20
#define MAX_OPEN_SCANS 20
#define SIZE_OF_FIRST_BLOCK 22 + 2 + 3 * sizeof(int) + 5 + 1
#define MAX_ENTRIES_NUM BF_BLOCK_SIZE/(curr_file->attrLength1 + sizeof(int) + 1 + 1) + 4 + 3*sizeof(int)
#define field_sep "$"
#define entry_sep "&"

int AM_errno = AME_OK;
typedef struct File
{
        int fileDesc;
		char* fileName;
		int rootId;
		char attrType1;
		char attrType2;
		int attrLength1;
		int attrLength2;

}File;

typedef struct Scan
{
		int fileDesc;
        int scanDesc;
        char* Results[100];	//char** Results we have to see about the size of the results array,
        					//normally we have a dynamic size of the array Results
		int index;
}Scan;

int compareKeys(File curr_file, char* value, char* value1)
{
    if ((curr_file.attrType1 == 'i' && atoi(value) == atoi(value1)) ||
     (curr_file.attrType1 == 'f' && atof(value) == atof(value1)) ||
     (curr_file.attrType1 == 'c' && strcmp(value, value1) == 0))
        return 0;
    else if ((curr_file.attrType1 == 'i' && atoi(value) > atoi(value1)) ||
     (curr_file.attrType1 == 'f' && atof(value) > atof(value1)) ||
     (curr_file.attrType1 == 'c' && strcmp((char*)value, value1) > 0))
        return 1;
    else
        return -1;
}

void sort2arrays(char** array1, char** array2, int n, File curr_file)
{
    char* temp;
    for (int i = 0; i < n; i++)
    {
        for (int j = n - 1; j > i; j--)
        {
            if ((curr_file.attrType1 == 'i' && atoi(array1[i]) > atoi(array1[j])) ||
			 (curr_file.attrType1 == 'f' && atof(array1[i]) > atof(array1[j])) ||
			 (curr_file.attrType1 == 'c' && strcmp(array1[i], array1[j]) > 0))
             {
                 // swap
                 temp = array1[i];
                 array1[i] = array1[j];
                 array1[j] = temp;

                 temp = array2[i];
                 array2[i] = array2[j];
                 array2[j] = temp;
             }
        }
    }
}

void sort2arraysInt(char** array1, int* array2, int n, File curr_file)
{
    char* temp;
    for (int i = 0; i < n; i++)
    {
        for (int j = n - 1; j > i; j--)
        {
            if ((curr_file.attrType1 == 'i' && atoi(array1[i]) > atoi(array1[j])) ||
			 (curr_file.attrType1 == 'f' && atof(array1[i]) > atof(array1[j])) ||
			 (curr_file.attrType1 == 'c' && strcmp(array1[i], array1[j]) > 0))
             {
                 // swap
                 temp = array1[i];
                 array1[i] = array1[j];
                 array1[j] = temp;

                 int tempI;
                 tempI = array2[i];
                 array2[i] = array2[j];
                 array2[j] = tempI;
             }
        }
    }
}

// finds the data block in which the new entry (value1, value2) belongs and returns its blockId
int findSuitableBlockId(int blockId, void* value1, File* curr_file, int fileDesc)
{
	BF_Block* block;
	BF_Block_Init(&block);
	char* blockData;

	if (BF_GetBlock(fileDesc, blockId, block) != BF_OK)
		return -1;

	blockData = BF_Block_GetData(block);

	if (blockData[0] != ':' && blockData[1] != ')') // if current block is index block
	{
		char* temp = malloc(BF_BLOCK_SIZE);
        // copy blockData to manipulate a copy of them
		strcpy(temp, blockData);

		char* blockDataEntries[MAX_ENTRIES_NUM]; // entries in a block
  		char* blockDataFields[2]; // fields of an entry

  		blockDataEntries[0] = strtok(temp, "&"); // first entry of index block is the parent blockId and entries number

  		int i = 1;
  		while (blockDataEntries[i - 1] != NULL && i < MAX_ENTRIES_NUM) // get the rest of the key-index couples (P0, K1, P1, K2, P2...)
  		{
  			blockDataEntries[i] = strtok(NULL, "&");
  			i++;
		}

        // keep first blockId to go to next block (P0) (in case value1 < K1)
		int blockIdNext = atoi(blockDataEntries[1]); // blockIdNext: the block id of the next block to go to, to traverse the B+ tree

		int found = 0;
		i = 2; // start from 3rd entry (K1$P1)
		while (blockDataEntries[i] != NULL && i < MAX_ENTRIES_NUM)
		{
			blockDataFields[0] = strtok(blockDataEntries[i], "$"); // get key value (Ki)

			blockDataFields[1] = strtok(NULL, "&"); // get blockId-index (Pi)

			// if value1 < K_i call the function recursively for the previous blockId
			if ((curr_file->attrType1 == 'i' && *((int*)value1) < atoi(blockDataFields[0])) ||
			 (curr_file->attrType1 == 'f' && *((float*)value1) < atof(blockDataFields[0])) ||
			 (curr_file->attrType1 == 'c' && strcmp((char*)value1, blockDataFields[0]) < 0))
			 {
                if (BF_UnpinBlock(block) != BF_OK)
                    return -1;
				BF_Block_Destroy(&block);
				found = 1;
				findSuitableBlockId(blockIdNext, value1, curr_file, fileDesc);
				break;
			  }

			blockIdNext = atoi(blockDataFields[1]);
			i++;
		}
		if (found == 0) // if value1 > Kn(which is the last value in block) go to block with blockId = Pn(which is the last blockId in block)
		{
			BF_Block_Destroy(&block);
			findSuitableBlockId(blockIdNext, value1, curr_file, fileDesc);
		}
        free(temp);
  	}
  	else if (blockData[0] == ':' && blockData[1] == ')' && blockId != 0) // if current block is data block return the blockId
    {
        if (BF_UnpinBlock(block) != BF_OK)
            return -1;
        BF_Block_Destroy(&block);
        return blockId;
    }
}

// split data block and insert the entry(value1, value2) doing all necessary splits in higher index blocks
int split(int blockId, int fileDesc, File* curr_file, void* value1, void* value2, char* indexValue, int childBlockId)
{
    BF_Block* block;
    BF_Block_Init(&block);

    if (BF_GetBlock(fileDesc, blockId, block) != BF_OK)
        return -1;

    char* blockData = BF_Block_GetData(block);
    // copy blockData to manipulate a copy of them
    char* temp = malloc(BF_BLOCK_SIZE);
    strcpy(temp, blockData);

    char* blockDataEntries[MAX_ENTRIES_NUM]; // entries in the block
    char* blockDataFields[3]; // fields of an entry

    int parentBlockId; // the blockId of the parent block (useful for traversing the tree from bottom up to split blocks)
    int entriesNum; // number of entries in the block (if it is index block, entriesNum includes P0)
    int blocksNum; // number of blocks in the file

    // get all entries of the block
    blockDataEntries[0] = strtok(temp, "&");
    int i = 1;
    while (blockDataEntries[i - 1] != NULL && i < MAX_ENTRIES_NUM)
    {
        blockDataEntries[i] = strtok(NULL, "&");
        i++;
    }


    if (strcmp(blockDataEntries[0], ":)") != 0) // if current block is index block
    {
        // get block's metadata
        blockDataFields[0] = strtok(blockDataEntries[0], "$"); // parent blockId
        blockDataFields[1] = strtok(NULL, "&"); // entries number
        char* parentBlockIdString = malloc(sizeof(int) + 1);
        strcpy(parentBlockIdString, blockDataFields[0]);
        entriesNum = atoi(blockDataFields[1]);


        char* values[entriesNum + 1 - 1]; // values: Ki (-1: because first entry is only blockId (P0))
        int blockIds[entriesNum + 1]; // blockIds: Pi (including first entry (P0))

        i = 1;
        int j = 0;
        // get the indexes and values from the block (P0, K1, P1, K2, P2, ...)
        while (blockDataEntries[i] != NULL)
        {
            if (i == 1)
            {   // get only P0
                blockDataFields[0] = strtok(blockDataEntries[i], "$"); // P0
                blockIds[j] = atoi(blockDataFields[0]);
            }
            else
            {   // get the (value, index) pair
                blockDataFields[0] = strtok(blockDataEntries[i], "$"); // Ki
                blockDataFields[1] = strtok(NULL, "&"); // Pi
                values[j - 1] = blockDataFields[0];
                blockIds[j] = atoi(blockDataFields[1]);
            }
            i++;
            j++;
        }
        // add the new entry
        values[entriesNum - 1] = indexValue;
        blockIds[entriesNum] = childBlockId;

        // get all blockIds not including P0 because it doesn't change place (for sorting 2 arrays)
        int someBlockIds[entriesNum]; // blockIds without P0
        for (i = 0; i < entriesNum; i++)
            someBlockIds[i] = blockIds[i + 1];

        // sort values and someBlockIds with ascending order of values
        sort2arraysInt(values, someBlockIds, entriesNum, *curr_file);

        // update blockIds array which contains P0
        for (i = 1; i < entriesNum + 1; i++)
            blockIds[i] = someBlockIds[i - 1];

        if (strlen(blockData) + curr_file->attrLength1 + 1 + sizeof(int) + 1 <= BF_BLOCK_SIZE) // entry fits in block
        {
            // reconstruct the index block
            char* data = malloc(2*sizeof(int) + 2);
            // construct the metadata and P0
            sprintf(data, "%s$%d&%d&", parentBlockIdString, entriesNum + 1, blockIds[0]); // update entriesNum
            strcpy(blockData, data);

            char* entry = malloc(curr_file->attrLength1 + 1 + sizeof(int) + 1);
            // construct entries and insert them to the block
            for(i = 1; i < entriesNum + 1; i++)
            {
                sprintf(entry, "%s$%d&", values[i-1], blockIds[i]);
                strcat(blockData, entry);
            }

            BF_Block_SetDirty(block);
            if (BF_UnpinBlock(block) != BF_OK)
                return -1;

            BF_Block_Destroy(&block);
            free(entry);
            free(data);
        }
        else // entry doesn't fit in block
        {
            // start splitting the index block
            // the data of the first half of the index block will be inserted in blockData
            strcpy(blockData, ""); // empty "old" block (unnecessary but it is for clarification reasons)

            BF_Block* block1;
            BF_Block_Init(&block1);

            // allocate a new block to insert the second half of the entries of the old block
            if (BF_AllocateBlock(fileDesc, block1) != BF_OK)
                return -1;

            char* blockData1 = BF_Block_GetData(block1);

            if (strcmp(parentBlockIdString, "-") != 0) // the old index block had a parent block
            {
                parentBlockId = atoi(parentBlockIdString); // get parentBlockId
                // call split recursively for the parent block, to insert the values[(entriesNum/2) + 1] (index value)
                // and blockIds[((entriesNum + 1)/2)] (index block id)
                int error = split(parentBlockId, fileDesc, curr_file, value1, value2, values[(entriesNum/2) + 1],
                 blockIds[((entriesNum + 1)/2)]);
                if (error == -1)
                    return -1;
            }
            else // the old index block didn't have a parent block
            {
                // create parent index block
                BF_Block* parentBlock;
                BF_Block_Init(&parentBlock);

                if (BF_AllocateBlock(fileDesc, parentBlock) != BF_OK)
                    return -1;

                char* parentBlockData = BF_Block_GetData(parentBlock);

                if (BF_GetBlockCounter(fileDesc, &blocksNum) != BF_OK)
                    return -1;

                // update rootId
                curr_file->rootId = blocksNum - 1;
                // update parentBlockId
                parentBlockId = blocksNum - 1;

                char* data = malloc(3*sizeof(int) + curr_file->attrLength1 + 7); // 7 is for the delimiters ("$" and "&")
                // construct the metadata and the 2 first entries (P0, [K1, P1])
                sprintf(data, "-$%d&%d&%s$%d&", 2, childBlockId, values[(entriesNum/2) + 1],
                 blockIds[((entriesNum + 1)/2)]); // insert parentBlockId("-"), entriesNum(2), P0, K1, P1
                                                  // "-": no parent yet
                strcpy(parentBlockData, data);

                BF_Block_SetDirty(parentBlock);
                if (BF_UnpinBlock(parentBlock) != BF_OK)
                    return -1;

                BF_Block_Destroy(&parentBlock);
                free(data);
            }

            // construct the first(left) child index block (block, blockData)
            char* data = malloc(sizeof(int) + 1 + sizeof(int) + 1 + 1);

            // metadata
            sprintf(data, "%d$%d&%d&", parentBlockId, (entriesNum + 1)/2, blockIds[0]);

            strcpy(blockData, data);
            char* entry = malloc(curr_file->attrLength1 + 1 + sizeof(int) + 1 + 1);
            // insert half of the entries
            for (i = 1; i < (entriesNum + 1)/2; i++)
            {
                // create entry
                sprintf(entry, "%s$%d&", values[i-1], blockIds[i]);
                strcat(blockData, entry);
            }

            BF_Block_SetDirty(block);
            if (BF_UnpinBlock(block) != BF_OK)
                return -1;

            BF_Block_Destroy(&block);

            // construct the second(rigth) child index block (block1, blockData1)
            int newEntriesNum;
            // calculate the new entries number for the second child block
            if ((entriesNum + 1)%2 == 0)
                newEntriesNum = (entriesNum + 1)/2 - 1;
            else
                newEntriesNum = (entriesNum + 1)/2;

            // metadata
            sprintf(data, "%d$%d&%d&", parentBlockId, newEntriesNum, blockIds[(entriesNum + 1)/2 + 1]); // insert P0

            strcpy(blockData1, data);

            // insert half of the entries skipping one because we are splitting an index block
            for (i = (entriesNum + 1)/2 + 2; i < entriesNum + 1; i++)
            {
                // create entry
                sprintf(entry, "%s$%d&", values[i-1], blockIds[i]);
                strcat(blockData1, entry);
            }

            BF_Block_SetDirty(block1);
            if (BF_UnpinBlock(block1) != BF_OK)
                return -1;

            BF_Block_Destroy(&block1);
            free(data);
            free(entry);
        }
    }
    else // current block is a data block so it must be splitted
    {
        // get block's metadata
        char* parentBlockIdString = malloc(sizeof(int) + 1);
        blockDataFields[0] = strtok(blockDataEntries[1], "$"); // parent block id
        strcpy(parentBlockIdString, blockDataFields[0]);
        blockDataFields[1] = strtok(NULL, "$"); // entries number
        blockDataFields[2] = strtok(NULL, "&"); // next block's id

        entriesNum = atoi(blockDataFields[1]);
        char* nextBlockId = malloc(sizeof(int));
        strcpy(nextBlockId, blockDataFields[2]); // nextBlockId may be "-" instead of an int


        char* values1[entriesNum + 1]; // "+ 1" is for the entry to be inserted
        char* values2[entriesNum + 1];

        i = 2;
        int j = 0;
        // get the entries of the block (value1, value2)
        while (blockDataEntries[i] != NULL)
        {
            blockDataFields[0] = strtok(blockDataEntries[i], "$"); // value1
            blockDataFields[1] = strtok(NULL, "&"); // value2

            values1[j] = blockDataFields[0];
            values2[j] = blockDataFields[1];
            j++;
            i++;
        }
        // add the value to be inserted
        values1[entriesNum] = (char*) value1;
        values2[entriesNum] = (char*) value2;

        // sort values1 and values2 with ascending order of values1
        sort2arrays(values1, values2, entriesNum + 1, *curr_file);

        // empty the "old" block (unnecesary command but it is for clarification)
        // the data of the first half of the data block will be inserted in blockData
        strcpy(blockData, "");

        BF_Block* block1;
        BF_Block_Init(&block1);

        // create a new block to insert the second half of the entries of the old block (used later)
        if (BF_AllocateBlock(fileDesc, block1) != BF_OK)
            return -1;

        int blocksNum;
        if (BF_GetBlockCounter(fileDesc, &blocksNum) != BF_OK)
            return -1;

        int nextBlockId1 = blocksNum - 1; // next block's id for the first(left) child data block after the splitting
        char* blockData1 = BF_Block_GetData(block1);

        if (strcmp(parentBlockIdString, "-") == 0) // no parent block which means this is the first and the only block(data block) in the file
        {
            // create a parent index block
            BF_Block* parentBlock;
            BF_Block_Init(&parentBlock);

            if (BF_AllocateBlock(fileDesc, parentBlock) != BF_OK)
                return -1;

            char* parentBlockData = BF_Block_GetData(parentBlock);

            if (BF_GetBlockCounter(fileDesc, &blocksNum) != BF_OK)
                return -1;

            // update parentBlockId
            parentBlockId = blocksNum - 1;
            // update rootId
            curr_file->rootId = parentBlockId;

            char* data = malloc(3*sizeof(int) + curr_file->attrLength1 + 7);
            // construct the metadata
            sprintf(data, "-$%d&%d&%s$%d&", 2, 1, values1[entriesNum/2], nextBlockId1); // insert parentBlockId("-"), entriesNum(2), P0, K1, P1
                                                                                        // "-": no parent block yet
            strcpy(parentBlockData, data);

            BF_Block_SetDirty(parentBlock);
            if (BF_UnpinBlock(parentBlock) != BF_OK)
                return 1;

            BF_Block_Destroy(&parentBlock);
            free(data);
        }
        else // there is a parent index block for the current data block
        {
            parentBlockId = atoi(parentBlockIdString);
            int error;
            error = split(parentBlockId, fileDesc, curr_file, value1, value2, values1[entriesNum/2], nextBlockId1);
            if (error == -1)
                return -1;
        }

        // construct the first(left) child data block (block, blockData)
        char* data = malloc(2 + 1 + sizeof(int) + 1 + sizeof(int) + 1 + sizeof(int) + 1);
        // metadata
        sprintf(data, ":)&%d$%d$%d&", parentBlockId, (entriesNum+1)/2, nextBlockId1);
        strcpy(blockData, data);

        char* entry = malloc(curr_file->attrLength1 + 1 + curr_file->attrLength2 + 1);
        // insert the first half of the entries
        for (i = 0; i < (entriesNum + 1)/2; i++)
        {
            // create entry
            sprintf(entry, "%s$%s&", values1[i], values2[i]);
            strcat(blockData, entry);
        }

        BF_Block_SetDirty(block);
        if (BF_UnpinBlock(block) != BF_OK)
            return -1;

        BF_Block_Destroy(&block);

        // construct the second(right) child data block (block1, blockData1)
        int newEntriesNum;
        // calculate a new entries number for the right child block
        if((entriesNum+1)%2 == 0)
            newEntriesNum = (entriesNum+1)/2;
        else
            newEntriesNum = (entriesNum+1)/2 + 1;

        // metadata
        sprintf(data, ":)&%d$%d$%s&", parentBlockId, newEntriesNum, nextBlockId);
        strcpy(blockData1, data);
        // insert the second half of the entries
        for (i = (entriesNum + 1)/2; i < entriesNum + 1; i++)
        {
            sprintf(entry, "%s$%s&", values1[i], values2[i]);
            strcat(blockData1, entry);
        }

        BF_Block_SetDirty(block1);
        if (BF_UnpinBlock(block1) != BF_OK)
            return -1;

        BF_Block_Destroy(&block1);
        free(data);
        free(entry);
    }
    free(temp);
    return 0;
}


File open_files[MAX_OPEN_FILES]; // global array of open files
Scan open_scans[MAX_OPEN_SCANS]; // global array of open scans

void AM_Init() {
    BF_Init(LRU);

    // initialize array of open files
    for(int i=0; i<MAX_OPEN_FILES; i++) {
        open_files[i].fileDesc=-1;
        open_files[i].fileName=NULL;
        open_files[i].rootId = -1;
        open_files[i].attrLength1 = -1;
        open_files[i].attrType1 = ' ';
    	open_files[i].attrType2 = ' ';
    	open_files[i].attrLength2 = -1;
    }

    // initialize array of open scans
	for(int i=0; i<MAX_OPEN_SCANS; i++) {
		open_scans[i].scanDesc=-1;
		open_scans[i].fileDesc = -1;
		open_scans[i].index=0;
		}
}

int AM_CreateIndex(char *fileName,
	               char attrType1,
	               int attrLength1,
	               char attrType2,
	               int attrLength2) {

    // check for validity of parameters
    if ((attrType1 == 'c' && (attrLength1 > 255 || attrLength1 < 1)) ||
        (attrType1 == 'i' && attrLength1 != 4) ||
        (attrType1 == 'f' && attrLength1 != 4))
        {
            printf("Invalid attrLength1\n");
            return -1;
        }

    if ((attrType2 == 'c' && (attrLength2 > 255 || attrLength2 < 1)) ||
        (attrType2 == 'i' && attrLength2 != 4) ||
        (attrType2 == 'f' && attrLength2 != 4))
        {
            printf("Invalid attrLength2\n");
            return -1;
        }

	if (BF_CreateFile(fileName) != BF_OK)
	{
		printf("The file with name: %s already exists\n", fileName);
		return -1; // error
	}

	int fileDesc;
	if (BF_OpenFile(fileName, &fileDesc) != BF_OK)
		return -1;

	BF_Block* block = malloc(BF_BLOCK_SIZE);
	if (BF_AllocateBlock(fileDesc, block) != BF_OK)
		return -1;

	char* blockData;
	blockData = BF_Block_GetData(block);

	char* s = malloc(SIZE_OF_FIRST_BLOCK); // "This is a B+ tree file" + attrType1 + attrLength1 + attrType2 + attrLength2
	char attrLen1[4], attrLen2[4];

	sprintf(attrLen1, "%d", attrLength1);
	sprintf(attrLen2, "%d", attrLength2);
    // insert metadata for the file
	sprintf(s, "This is a B+ tree file$%c$%s$%c$%s$%d", attrType1, attrLen1, attrType2, attrLen2, -1); // -1 is rootId

	strcpy(blockData, s);

	BF_Block_SetDirty(block);

	if (BF_UnpinBlock(block) != BF_OK)
		return -1;

	BF_Block_Destroy(&block);

    if (BF_CloseFile(fileDesc) != BF_OK)
		return -1;
	free(s);
	return AME_OK;
}


int AM_DestroyIndex(char *fileName) {

    // check if file is open
	for (int i = 0; i < MAX_OPEN_FILES; i++)
	{
        if (open_files[i].fileName != NULL)
        {
        	if (strcmp(open_files[i].fileName, fileName) == 0)
        	{
        		printf("File is open and cannot be deleted\n");
        		return -1;
        	}
        }
	}
    // create command to delete the file
	char* command = malloc(8 + strlen(fileName));
	sprintf(command, "rm -rf %s", fileName);

    // execute command
	system(command);

    free(command);
	return AME_OK;
}


int AM_OpenIndex(char *fileName) {

	int fileDesc;

	if (BF_OpenFile(fileName, &fileDesc) != BF_OK)
		return -1;

    BF_Block* block;
	BF_Block_Init(&block);

    // get the first block which contains the file's metadata
	if (BF_GetBlock(fileDesc, 0, block) != BF_OK)
		return -1;

	char* blockData;
	blockData = BF_Block_GetData(block);

	char* temp;
	temp = malloc(BF_BLOCK_SIZE);

	strcpy(temp, blockData);

    // check if it is a B+ tree file
	if (strcmp(strtok(temp, "$"), "This is a B+ tree file") != 0)
	{
		printf("This is not a B+ tree file\n");
		return -1;
	}

	char *attrLen1, *attrLen2, attrType1, attrType2, *rootIdString;

	attrType1 = strtok(NULL, "$")[0];
	attrLen1 = strtok(NULL, "$");
	attrType2 = strtok(NULL, "$")[0];
	attrLen2 = strtok(NULL, "$");
    rootIdString = strtok(NULL, "$");
    // insert the file information in the first available position of open_files array
	for (int i = 0; i < MAX_OPEN_FILES; i++)
	{
		if (open_files[i].fileDesc == -1)
		{
			open_files[i].fileDesc = fileDesc;
			open_files[i].fileName = malloc(sizeof(fileName));
			strcpy(open_files[i].fileName, fileName);
            open_files[i].rootId = atoi(rootIdString);
			open_files[i].attrType1 = attrType1;
			open_files[i].attrLength1 = atoi(attrLen1);
			open_files[i].attrType2 = attrType2;
			open_files[i].attrLength2 = atoi(attrLen2);

            if (BF_UnpinBlock(block) != BF_OK)
        		return -1;
            BF_Block_Destroy(&block);
            free(temp);
			return i; // return index for this file in open_files array
		}
	}
	return -1;
}


int AM_CloseIndex (int fileDesc) {

    for (int i = 0; i < 20; i++)
    {
        if (open_scans[i].fileDesc == open_files[fileDesc].fileDesc)
            return -1;
    }

    BF_Block* block;
    BF_Block_Init(&block);
    // get the metadata block of file (the first block) to update the root id
    if (BF_GetBlock(open_files[fileDesc].fileDesc, 0, block) != BF_OK)
        return -1;

    char* blockData = BF_Block_GetData(block);
    char* temp = malloc(BF_BLOCK_SIZE);
    strcpy(temp, blockData);

    char* descriptionBlockEntries[5];
    // get the metadata not including the root id (last entry)
    descriptionBlockEntries[0] = strtok(temp, "$");
    for (int i = 1; i < 5; i++)
        descriptionBlockEntries[i] = strtok(NULL, "$");

    // reconstruct the metadata block
    strcpy(blockData, "");
    for (int i = 0; i < 5; i++)
    {
        strcat(blockData, descriptionBlockEntries[i]);
        strcat(blockData, "$");
    }
    char* rootIdString = malloc(sizeof(int) + 1);
    // update root id
    sprintf(rootIdString, "%d", open_files[fileDesc].rootId);

    strcat(blockData, rootIdString);
    BF_Block_SetDirty(block);
    if(BF_UnpinBlock(block) != BF_OK)
        return -1;

    //BF_Block_Destroy(&block);
    free(temp);
    free(rootIdString);
    //printf("done\n");

    int blocksNum;
    if (BF_GetBlockCounter(open_files[fileDesc].fileDesc, &blocksNum) != BF_OK)
        return -1;

    // unpin all pinned blocks
    for (int i = 0; i < blocksNum; i++)
    {
        if (BF_GetBlock(open_files[fileDesc].fileDesc, i, block) != BF_OK)
            return -1;
        if (BF_UnpinBlock(block) != BF_OK)
            return -1;
    }

    BF_Block_Destroy(&block);
    // close the file
    if (BF_CloseFile(open_files[fileDesc].fileDesc) != BF_OK)
        return -1;

    // reinitialize file information
    open_files[fileDesc].rootId = -1;
	open_files[fileDesc].fileDesc = -1;
    free(open_files[fileDesc].fileName);
	open_files[fileDesc].fileName = NULL;
	open_files[fileDesc].attrLength1 = -1;
	open_files[fileDesc].attrType1 = ' ';
	open_files[fileDesc].attrType2 = ' ';
	open_files[fileDesc].attrLength2 = -1;

	return AME_OK;
}


int AM_InsertEntry(int fileDesc, void *value1, void *value2) {

    File *curr_file;
    // if file exists, get it
    if (fileDesc < 20)
        curr_file = &open_files[fileDesc];
    else
        printf("Invalid fileDesc\n");

	// check for validity of the given entry (value1, value2)
	if ((curr_file->attrType1 == 'c' && strlen((char*) value1) > curr_file->attrLength1) ||
        (curr_file->attrType1 == 'i' && sizeof(*(int*)value1) > curr_file->attrLength1) ||
        (curr_file->attrType1 == 'f' && sizeof(*(float*)value1) > curr_file->attrLength1))
	{
		printf("Invalid value1\n");
		return -1;
	}

    if ((curr_file->attrType2 == 'c' && strlen((char*) value2) > curr_file->attrLength2) ||
        (curr_file->attrType2 == 'i' && sizeof(*(int*) value2) > curr_file->attrLength2) ||
        (curr_file->attrType2 == 'f' && sizeof(*(float*) value2) > curr_file->attrLength2))
	{
		printf("Invalid value2\n");
		return -1;
	}


	// start the insert procedure
	int blocksNum;
	if (BF_GetBlockCounter(fileDesc, &blocksNum) != BF_OK)
		return -1;

	BF_Block *block;
	BF_Block_Init(&block);
	char* blockData;

	// create the entry to be inserted
	char* entry = malloc(curr_file->attrLength1 + 1 + curr_file->attrLength2 + 1 + 1);

    if (curr_file->attrType1 == 'c' && curr_file->attrType2 == 'c')
        sprintf(entry, "%s$%s&", (char*) value1, (char*) value2);
    else if (curr_file->attrType1 == 'c' && curr_file->attrType2 == 'i')
        sprintf(entry, "%s$%d&", (char*) value1, *(int*) value2);
    else if (curr_file->attrType1 == 'c' && curr_file->attrType2 == 'f')
        sprintf(entry, "%s$%f&", (char*) value1, *(float*) value2);
    else if (curr_file->attrType1 == 'i' && curr_file->attrType2 == 'c')
        sprintf(entry, "%d$%s&", *(int*) value1, (char*) value2);
    else if (curr_file->attrType1 == 'i' && curr_file->attrType2 == 'i')
        sprintf(entry, "%d$%d&", *(int*) value1, *(int*) value2);
    else if (curr_file->attrType1 == 'i' && curr_file->attrType2 == 'f')
        sprintf(entry, "%d$%f&", *(int*) value1, *(float*) value2);
    else if (curr_file->attrType1 == 'f' && curr_file->attrType2 == 'c')
        sprintf(entry, "%f$%s&", *(float*) value1, (char*) value2);
    else if (curr_file->attrType1 == 'f' && curr_file->attrType2 == 'i')
        sprintf(entry, "%f$%d&", *(float*) value1, *(int*) value2);
    else if (curr_file->attrType1 == 'f' && curr_file->attrType2 == 'f')
        sprintf(entry, "%f$%f&", *(float*) value1, *(float*) value2);

    char* entrycopy = malloc(curr_file->attrLength1 + 1 + curr_file->attrLength2 + 1 + 1);
    strcpy(entrycopy, entry);
    // get value1 and value2 again because in some cases they are corrupted
    value1 = strtok(entrycopy, "$");
    value2 = strtok(NULL, "&");

	if (blocksNum == 1) // only the description block exists
	{
		// create the first data block
		if (BF_AllocateBlock(fileDesc, block) != BF_OK)
			return -1;

		blockData = BF_Block_GetData(block);

		strcpy(blockData, ":)&-$1$-&"); // ":)": data block identifier, "-" is for invalid parent blockId
                                        // (because this is the only data block for now)
                                        //, "1" is the entries number for the block and "-" is for invalid next block's id
        // insert the first and the only entry
        strcat(blockData, entry);

        // update rootId
		curr_file->rootId = 1; // root is the new block (for now)

		BF_Block_SetDirty(block);
		if (BF_UnpinBlock(block) != BF_OK)
			return -1;

		BF_Block_Destroy(&block);
		free(entry);
        free(entrycopy);
    }
	else // there are more than 1 blocks in total in the file
	{
        // get the blockId for the block in which the given entry should be inserted
	    int blockId = findSuitableBlockId(curr_file->rootId, value1, curr_file, fileDesc);

		if (BF_GetBlock(fileDesc, blockId, block) != BF_OK)
            return -1;

        blockData = BF_Block_GetData(block);

        if (strlen(blockData) + curr_file->attrLength1 + 1 +
            curr_file->attrLength2 + 1 <= BF_BLOCK_SIZE) // entry fits in the data block with id blockId
        {
            // insert entry (later all entries of the data block will be sorted)
            strcat(blockData, entry);

            char* temp = malloc(BF_BLOCK_SIZE);
            // copy blockData to manipulate the copy
            strcpy(temp, blockData);

            char* blockDataEntries[MAX_ENTRIES_NUM]; // entries of the block1
            char* blockDataFields[3]; // fields of an entry
            char* parentBlockId = malloc(sizeof(int));
            int entriesNum;
            char* nextBlockId = malloc(sizeof(int));


            blockDataEntries[0] = strtok(temp, "&"); // ":)" skip it

            int i = 1;
            // get all entries of the data block
            while (blockDataEntries[i-1] != NULL && i < MAX_ENTRIES_NUM)
            {
                blockDataEntries[i] = strtok(NULL, "&");
                i++;
            }

            // get block's metadata
            blockDataFields[0] = strtok(blockDataEntries[1], "$"); // parentBlockId
            blockDataFields[1] = strtok(NULL, "$"); // entriesNum
            blockDataFields[2] = strtok(NULL, "&"); // nextBlockId
            strcpy(parentBlockId, blockDataFields[0]);
            entriesNum = atoi(blockDataFields[1]);
            strcpy(nextBlockId, blockDataFields[2]);

            // update entriesNum
            entriesNum++;
            char* values1[entriesNum]; // contains the first value of each entry
            char* values2[entriesNum]; // contains the second value of each entry

            i = 2; // skip ":)" and metadata
            int j = 0;
            // save all entries (value1, value2)
            while (blockDataEntries[i] != NULL)
            {
                blockDataFields[0] = strtok(blockDataEntries[i], "$");
                blockDataFields[1] = strtok(NULL, "&");

                values1[j] = blockDataFields[0];
                values2[j] = blockDataFields[1];

                i++;
                j++;
            }

            // sort values1 and values2 with ascending order of values1
            sort2arrays(values1, values2, entriesNum, *curr_file);

            // reconstruct the data block
            char* metadata = malloc(2 + 1 + sizeof(int) + 1 + sizeof(int) + 1 + sizeof(int) + 1);
            // construct the metadata
            sprintf(metadata, ":)&%s$%d$%s&", parentBlockId, entriesNum, nextBlockId); // entriesNum is already updated
            strcpy(blockData, metadata);

            // insert all entries sorted
            for (i = 0; i < entriesNum; i++)
            {
                sprintf(entry, "%s$%s&", values1[i], values2[i]);
                strcat(blockData, entry);
            }

            BF_Block_SetDirty(block);
            if (BF_UnpinBlock(block) != BF_OK)
                return -1;
            BF_Block_Destroy(&block);

            free(metadata);
            free(parentBlockId);
            free(nextBlockId);
            free(entry);
            free(entrycopy);
            free(temp);
        }
        else // entry doesn't fit in data block with id blockId
        {
            if (BF_UnpinBlock(block) != BF_OK)
                return -1;

            BF_Block_Destroy(&block);
            // call split for this data block
            int error = split(blockId, fileDesc, curr_file, value1, value2, "", -1);
            if (error == -1)
            {
                printf("error in split!");
                return -1;
            }
        }
	}
	return AME_OK;
}


int AM_OpenIndexScan(int fileDesc, int op, void *value) {

    File curr_file = open_files[fileDesc];
    char *data,*data2 = malloc(BF_BLOCK_SIZE);
    char* data1 = malloc(BF_BLOCK_SIZE);
    char* data3 = malloc(BF_BLOCK_SIZE);
    char *rec,*rec_key,*rec_value,*node,*metaData,*newNodeString;
    int newNode,pos,scanDesc,i,it=0;
    Scan scan;
    BF_Block* block;
    char* valuecopy=malloc(curr_file.attrLength2 +1);
    newNodeString=malloc(sizeof(int)+1);
    rec_key=malloc(curr_file.attrLength1 + 1 + curr_file.attrLength2 + 1 + 1);        //We commit memory tha we need to have in order to copy what we need to
    metaData=malloc(curr_file.attrLength1 + 1 + curr_file.attrLength2 + 1 + 1);

    BF_Block_Init(&block);

    for(int j = 0; j < 100; j++) {
        scan.Results[j] = malloc(curr_file.attrLength2 + 1);      //Commit memory also to store the results
        strcpy(scan.Results[j], "-");										//We pass to all results initially '-' and when we find the true results we pass them in to the array
    }

    if(curr_file.attrType1 == 'c')
        strcpy(valuecopy, (char*) value);
    else if (curr_file.attrType1 == 'i')							//Making a value copy casted to the appropriate type to compare with the block records
        sprintf(valuecopy,"%d",*(int*) value);
    else
        sprintf(valuecopy,"%f",*(float*) value);

    if (op == EQUAL)
        newNode = findSuitableBlockId(curr_file.rootId, value, &curr_file, curr_file.fileDesc);    //With this function we find the block we have to go next if we have equal and
    else                                                                                                                            //not begin from the root node(block)
        newNode=curr_file.rootId;

    do {
    //Getting every time the next block
    if (BF_GetBlock(curr_file.fileDesc, newNode, block) != BF_OK)
        return -1;
    //Taking the data from the block
    data=BF_Block_GetData(block);
    strcpy(data1,data);
    if(data1[0]==':'&& data1[1]==')') {   //Inside of the if is for data blocks
        i=3;//We initialize i=3 every time we go to a data block in order to take First record(key,value) and we increase it every time to go to the next record of a block
        strcpy(data2,data);
        rec=strtok(data1,entry_sep);
        rec=strtok(NULL,entry_sep);                                                        //With these strtok we take the metadata of each block (Parent block_id,Record counter,next block_id)
        strcpy(metaData,rec);
        rec=strtok(NULL,entry_sep);                                                        //Taking the first record
        node=strtok(metaData,field_sep);
        node=strtok(NULL,field_sep);                                                       //Here we extract the next block_id from the metaData to know when we have to stop taking blocks
        node=strtok(NULL,entry_sep);
        strcpy(newNodeString,node);
        newNode=atoi(newNodeString);
                                                              //make the block_id integer from a string
        memcpy(metaData,rec,sizeof(rec));
        bool flag=true;

        while(rec!=NULL && flag) {                                                            //Structure of entries in a data block:&key1$value1&key2$value2...we separate the fields(key,value) with $ and records with &
            rec_value=strchr(rec,'$');                                                       //With this command we take everything from rec that has after the $(included)
            rec_value=rec_value+1;                                                           // And with that command we drop the $ and that is the record value
            strcpy(rec_key, strtok(rec,field_sep));                                                   //So with this command we take the key that is behind the $
            if(op==EQUAL) {
                if(compareKeys(curr_file, rec_key,valuecopy)==0) {
                   //printf("(in if)Key is:%s|\n",rec_key);
                   //printf("(in if)given key is:%s|\n\n\n\n\n", (char*) value);
                   strcpy(scan.Results[it++],rec_value);
                }
                else if(compareKeys(curr_file, rec_key,valuecopy)>0) {
                	flag=false;
                }
            }
            else if(op==NOT_EQUAL) {
                if(compareKeys(curr_file, rec_key,valuecopy)!=0) {
                    strcpy(scan.Results[it++],rec_value);
                }
            }
            else if(op==LESS_THAN) {
                if(compareKeys(curr_file, rec_key,valuecopy)<0) {
                    strcpy(scan.Results[it++],rec_value);
                }
                else {
                 flag=false;
                }
            }
            else if(op==GREATER_THAN) {                                                            //Depending to operator(6 options) We compare the given key with the key of the respective record
                if(compareKeys(curr_file, rec_key,valuecopy)>0) {                                                  //For the 3 options of EQUAL LESS_THAN LESS_THAN_OR_EQUAL if we find a key that is more large than the given one we go to the next block
                    strcpy(scan.Results[it++],rec_value);                                          //because the records inside of the block are sorted there is no sense to search for the rest of the block
                }
            }
            else if(op==GREATER_THAN_OR_EQUAL) {
                if(compareKeys(curr_file, rec_key,valuecopy)>=0) {
                    strcpy(scan.Results[it++],rec_value);
                }
            }
            else{
                if(compareKeys(curr_file, rec_key,valuecopy)<=0) {
                    strcpy(scan.Results[it++],rec_value);
                }
                else {
                 flag=false;
                }
            }
            strcpy(data3,data2);                                                                //Here we find the next record from the block
            rec=strtok(data3,entry_sep);                                                        //We are passing the metadata and depending on which item we are(determined by i)
            for(int record=0; record<i; record++)
                rec=strtok(NULL,entry_sep);
            i++;
            //With the while we iterate through records in the same block
        }
    //End of data block
    }
    else {//For index blocks:Structure of index block:metadata,&PO&P1$K1&P2$K2 ... we separate the P from k with $ and the whole pand k from each other with & as we did with the key values in the data blocks
        rec=strtok(data1,entry_sep);
        strcpy(newNodeString, strtok(NULL,entry_sep));
        newNode=atoi(newNodeString);
    }
    //End of index block
    } while(strcmp(newNodeString,"-")!=0);//With the do while we change blocks,If we have - it means that this is the last block

                                                             //Calling the functions for the block we do not need anymore

    BF_Block_Destroy(&block);

    // insert information for the scan in the first available position of open_scans array
    for(int scanDesc=0; scanDesc<MAX_OPEN_SCANS; scanDesc++) {
        if(open_scans[scanDesc].scanDesc==-1){
            for (int j = 0; j < 100; j++)
                open_scans[scanDesc].Results[j] = scan.Results[j];                                  //And here we pass what we have to keep(Results of query,scanDesc,fileDesc) in the open_scans array

            open_scans[scanDesc].scanDesc = scanDesc;
            open_scans[scanDesc].fileDesc = curr_file.fileDesc;

            // freeing allocated memory
            free(data1);
            free(data2);
            free(data3);
            free(valuecopy);
            free(rec_key);
            free(metaData);
            free(newNodeString);
            return scanDesc;						//Return the scanDesc which is the index of the array that we put in the scan record
        }
    }
    return -1;
}


void *AM_FindNextEntry(int scanDesc) {

	if(open_scans[scanDesc].index < 100) // if there are remaining results for this scan
	{
         if (strcmp(open_scans[scanDesc].Results[open_scans[scanDesc].index], "-") != 0) // if the next result is valid
         {
            void* result;
            // reformat the result to void* and advance the open_scans[scanDesc].index
            if (open_files[open_scans[scanDesc].fileDesc].attrType2 == 'c')
                result = (void*) open_scans[scanDesc].Results[open_scans[scanDesc].index++];
            else if (open_files[open_scans[scanDesc].fileDesc].attrType2 == 'i')
            {
                int res = atoi(open_scans[scanDesc].Results[open_scans[scanDesc].index++]);
                result = (void*) &res;
            }
            else
            {
                float res = atof(open_scans[scanDesc].Results[open_scans[scanDesc].index++]);
                result = (void*) &res;
            }
            return result;
        }
         else // there aren't any other valid results
         {
            AM_errno=AME_EOF;
            return (void*) NULL;
        }
    }
	else //  there aren't any other results
	{
        AM_errno=AME_EOF;
	    return (void*) NULL;
	}
	return (void*) NULL;
}


int AM_CloseIndexScan(int scanDesc) {
    // reinitialize scan information
    open_scans[scanDesc].scanDesc = -1;
    open_scans[scanDesc].fileDesc = -1;
    open_scans[scanDesc].index = 0; // initialize to 0 for the next scan that will be in this position
    for (int i = 0; i < 100; i++)
        open_scans[scanDesc].Results[i] = NULL;

    return AME_OK;
}


void AM_PrintError(char *errString) {
	printf("%s", errString);
  	printf("The error code is %d\n",AM_errno);	//Very simple function just to type the error and error code
}												//and to update in the other functions the global AM_errno variable

void AM_Close() {
	for(int i=0; i<MAX_OPEN_FILES; i++) {
		free(open_files[i].fileName);
		open_files[i].fileName=NULL;
  	}

	for(int j=0; j<MAX_OPEN_SCANS; j++) {
		for (int z=0; z<100; z++)
        {
            free(open_scans[j].Results[z]);
        	open_scans[j].Results[z]=NULL;
        }
    }
    BF_Close();
}
