#include <iostream>
#include <string>

using namespace std;

int main()
{
    string str1 = "TSU ";
    string str2 = "IAMCS";

    str1.append(str2);
    cout << "str1 + str2: " << str1 << endl;

    int len = str2.length();
    cout << "length of 'IAMCS': " << len << endl;

    int pos = str1.find("U");
    cout << "position of the letter 'U' in str1: " << pos+1;
}
