#pragma once
#include <iostream>
using namespace std;

class CStr
{
    char* string;
    char* generate(int length);

public:
    CStr();
    CStr(const char* str);
    CStr(int length);
    CStr(const CStr& obj);
    ~CStr();

    CStr& operator=(CStr& obj);
    CStr& operator=(char* str);
    bool operator>(CStr& obj);
    CStr& operator=(const CStr& obj);
    bool operator==(CStr& obj);
    bool operator==(const CStr& obj) const;
    bool operator<(const CStr& obj) const;

    int get_length() const;

    friend ostream& operator<<(ostream& stream, CStr& obj);
};


