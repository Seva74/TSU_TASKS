#include <iostream>
using namespace std;

long long s1, s2, s3, s4;
int ma, mi, minz, a[3];
int main()
{
    for (int i = 0; i < 3; i++)
    {
        cin >> a[0] >> a[1] >> a[2];
        ma = a[i];
        mi = 10000;
        for (int j = 0; j < 3; j++)
        {
            if (a[j] > ma) ma = a[j];
            if (a[j] < mi) mi = a[j];
        }
        s3 += ma;
        s1 += mi;
        s4 = a[0] + a[1] + a[2] - ma - mi;
        s2 += s4;
        if ((ma - s4) % 2 != 0 && (ma - s4) < mi) minz = ma - s4;
        if ((ma - mi) % 2 != 0 && (ma - mi) < minz) minz = ma - mi;
        if ((s1 + s2) % 2 != 0) cout << s3;
        else s3 -= mi;
    }
    cout << s3;
}
