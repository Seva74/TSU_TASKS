#include <iostream>
#include <fstream>
using namespace std;
int s, k1, k2, c;
char a;
int main()
{
    ifstream in("24.txt");

    while (in >> a)
    {
        if (a == 'D')
        {
            c++;
            k2++;
        }
        else if (c == 1) k2++;
        else k1++;
        if (c == 2)
        {
            if (s < k1 + k2)s = k1 + k2;
            c = 1;
            k1 = k2;
            k2 = 0;
        }

    }

    cout << s;


}