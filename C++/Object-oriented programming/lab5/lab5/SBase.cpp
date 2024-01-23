#include "SBase.h"



SFile::SFile(const char* filename)
{
    inputFile.open(filename);
}

SFile::~SFile()
{
    inputFile.close();
}

int SFile::Get()
{
    int number;
    if (inputFile >> number) {
        return number;
    }
    else {
        return -1;
    }
}



int* SQueue::generateArray(int length)
{
    int* result = new int[length];
    for (int i = 0; i < length; i++) {
        result[i] = rand() % 50;
    }
    this->length = length;
    currentQueue = 0;
    return result;
}

SQueue::SQueue(int length)
{
    queue = generateArray(length);
}

int SQueue::Get()
{
    if (currentQueue == length - 1) {
        return -1;
    }
    int result = queue[currentQueue];
    currentQueue++;
    return result;
}


int SKbrd::Get()
{
    int number;
    cin >> number;
    if (cin.fail()) {
        cin.clear();
        cin.ignore();
        return -1;
    }
    return number;
}