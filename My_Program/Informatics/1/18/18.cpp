#include <iostream>
using namespace std;
int main()
{
    int a, b, c, a1, b1, c1, x, y;
    cin >> a >> b >> c >> a1 >> b1 >> c1;
    x = a * 60 * 60 + b * 60 + c;
    y = a1 * 60 * 60 + b1 * 60 + c1;
    cout << y - x;
    return 0;
}