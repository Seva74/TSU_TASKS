#include <iostream>
#include <cmath>
using namespace std;
long long a;
int b, fir, sec, c;

int main()
{
    for (int i = 174457; i <= 174505; i++)
    {
        for (int j = 2; j <= i/2; j++)
        {
            if (i % j == 0)
            {
                j /= i;
                b++;
                if (fir == 0)fir = j;
                else sec = j;
            }
            if (b > 2)
            {
                j = 2;
                fir = 0;
                sec = 0;
                i++;
            }
        }
        if (b == 2)cout << fir << sec;
        b = 0;
        fir = 0;
        sec = 0;
    }
}

