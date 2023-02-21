#include <iostream>
using namespace std;
int main()
{
    int a, b;
    cin >> a;
    b = a * 45 + a / 2 * 5 + (a - 1) / 2 * 15;
    cout << (9 + b / 60) << ' ' << (b % 60);
        return 0;
}