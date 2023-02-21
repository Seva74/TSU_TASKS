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
            l[i][j] = i * j;
            cout << l[i][j] << " ";
        }
        cout << "\n";
    }
}