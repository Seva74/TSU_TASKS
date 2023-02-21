#include <iostream>
#include <fstream>

using namespace std;
int n, a, b, c, ma, k, s;
int main()
{
    fstream in("27-10b.txt");
    in >> n;
    k = 10000;
    for (int i = 0; i < n; i++)
    {
        in >> a >> b >> c;
            s += max(max(a, b), max(b, c));
        if (a % 4 != 0 && a != max(max(a, b), max(b, c)) && max(max(a, b), max(b, c)) - a < k) k = a;
        if (b % 4 != 0 && b != max(max(a, b), max(b, c)) && max(max(a, b), max(b, c)) - b < k) k = b;
        if (c % 4 != 0 && c != max(max(a, b), max(b, c)) && max(max(a, b), max(b, c)) - c < k) k = c;
    }
    if (s % 4 == 0)s -= k;
    cout << s;
}