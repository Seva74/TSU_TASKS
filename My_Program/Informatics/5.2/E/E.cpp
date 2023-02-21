#include <iostream>

using namespace std;
int n, m, a, b, d, e;
int l[100][100];
int main()
{
    cin >> n >> m;
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {
            cin >> l[i][j];
        }
    }

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {
            if (j == 0) {
                d = 0;
            }
            d += l[i][j];
            if (l[i][j] > a)
            {
                a = l[i][j];
                b = i;
                e = d;
            }
        }
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {   
            if (j == 0) {
                d = 0;
            }
            d += l[i][j];
            if (l[i][j] == a)
            {
                if (d>e) b = i;


            }
        }
    }

    cout << b ;

}