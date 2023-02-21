#include <iostream>

using namespace std;
int a, b, c, d;
int main()
{
    cin >> a >> b >> c >> d;
    while (a <= b) {
        if (a % d == c) cout << a << " ";
        a++;
    }
    return 0;
}
