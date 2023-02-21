#include <iostream>

using namespace std;
int n, m, a, b;
int l[100][100];
int main()
{
    cin >> n >> m;
    for (int i = 1; i <= n; i++)
    {
        if (i % 2 == 0)
        {
            a = m*i - 1;
            for (int j = m; j > 0; j--)
            {
                if (a / 10 == 0)cout << "   ";
                else if (a / 100 == 0) cout << "  ";
                else if (a / 1000 == 0) cout << " ";
                cout << a;
                a -= 1;
            }
            cout << "\n";
        }
        else
        {
            a = m * (i - 1);
            for (int j = 1; j < m+1; j++)
            {
                if (a / 10 == 0)cout << "   ";
                else if (a / 100 == 0) cout << "  ";
                else if (a / 1000 == 0) cout << " ";
                cout << a;
                a += 1;
            }
            cout << "\n";
        }
    }
}