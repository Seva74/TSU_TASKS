#include <iostream>

using namespace std;
int c, z;
int main()
{
    for (int a = 1; a <= 10000; a++)
    {
        for (int x = 1; x <= 10000; x++)
        {
            if ((((x % a) != 0) || ((x % 36) == 0) && ((x % 126) == 0) && (a > 1000)) == 0)
            {
                c = 0;
                break;
            }
        }
        if (c == 1)    cout << a<<'\n';
        c = 1;
    }

}