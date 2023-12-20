#pragma once
#include "Cstr.h"
#include <iostream>
using namespace std;

class CStrArray
{
	CStr* arr;
	int length;
public:
	CStrArray(int leng);
	~CStrArray();
	CStr& operator[](int index);
	void sort_by_content();
	void sort_by_length();
	int bin_search(const CStr& str);
	bool check_sort();
	friend ostream& operator<<(ostream& stream, CStrArray& obj);
};


