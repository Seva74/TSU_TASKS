#include <iostream>
#include <fstream>

using namespace std;
int n, s;
int main()
{
    ifstream in("27-9a.txt");
    in >> n;
    int a[n];
    s = -1;
    for (int i = 0; i < n; i++)
    {
        in >> a[i];
            if (i > 5 && a[i] % 2 == 0)
            {
                for (int j = 1; j <= 6; j++)
                {
                    if (a[i - j] % 2 == 1 && a[i] * a[i - j] < s) s = a[i] * a[i - j];
                }
            }
            if (i > 5 && a[i] % 2 == 1)
            {
                for (int j = 1; j <= 6; j++)
                {
                    if (a[i - j] % 2 == 0 && a[i] * a[i - j] < s) s = a[i] * a[i - j];
                }
            }
    }
    cout << s;

}
