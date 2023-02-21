#include <iostream>
using namespace std;
int main()
{
    int a, b, c, d;
    cin >> a >> b;
    c = a;
    d = b;
    a = d;
    b = c;
    cout << a << " " << b;
    return 0;
}