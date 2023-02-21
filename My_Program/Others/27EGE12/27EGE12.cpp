#include <iostream>
#include <fstream>

using namespace std;
int n, s;
int main()
{
    fstream in("27-12a.txt");
    cin >> n;
    int a[n];
    for (int i = 0; i < n; i++)
    {
        cin >> a[i];
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (j == i)j++;
            if (a[i] * a[j] % 6 == 0)s++;
        }
    }
    cout << s / 2;
}
