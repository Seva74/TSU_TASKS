#include <iostream>

using namespace std;
int n, m, a, b, d, sum;
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
            if (l[i][j] > a)
            {
                a = l[i][j];
            }
        }
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {
            if (l[i][j] == a) {
                sum++;
                j = m;
            }
        }
    }
    cout << sum << "\n";
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {
            if (l[i][j] == a) {
                cout << i << " ";
                j = m;
            }
        }
    }

}