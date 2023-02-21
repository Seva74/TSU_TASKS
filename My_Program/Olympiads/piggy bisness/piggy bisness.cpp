#include <iostream>
using namespace std;
int n, m, w, d, e, c;

int main()
{
    cin >> n >> m >> w;
    int a[n], b[m];
    for (int i = 1; i <= n; i++)
    {
        cin >> a[i];
    }
    for (int i = 1; i <= m; i++)
    {
        cin >> b[i];
    }
    int mi = 1000000;
    for (int i = 1; i <= n; ++i)
    {
        if (a[i] < mi) mi = a[i];
    }
    d = mi * w;
    for (int i = 1; i <= m; i++)
    {
        if (d < b[i])
        {
            e += b[i];
            c++;
        }
    }
    cout << e - (c * d);
}
