#pragma once
#include <iostream>
#include <map>
#include "SBase.h"
using namespace std;

class Freq {
private:
	map<int, int> counts;
	int sum;
public:
	void Calc(SBase& object);
	friend ostream& operator<<(ostream& stream, Freq& object);
};


class Diap : public Freq {
private:
	int min, max, count;
public:
	void Calc(SBase& container);
	friend ostream& operator<<(ostream& stream, Diap& object);
};