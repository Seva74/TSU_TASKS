#pragma once
#include <iostream>
#include <fstream>
#include <string>
using namespace std;


class SBase {
public:
	virtual int Get() { return 0; };
};

class SKbrd : public SBase {
public:
	int Get();
};

class SFile : public SBase {
private:
	std::ifstream inputFile;
public:
	SFile(const char* filename);
	~SFile();
	int Get() override;
};


class SQueue : public SBase {
private:
	int* queue;
	int length;
	int currentQueue;
	int* generateArray(int length);
public:
	SQueue(int length);
	int Get() override;
};
