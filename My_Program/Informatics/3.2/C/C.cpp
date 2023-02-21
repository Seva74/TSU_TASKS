#include <iostream>

using namespace std;
int c;
int a, b;
int main()
{
    cin >> a >> b;
    while (a <= b) {
        c = sqrt(a) * 10;
        if (c % 10 == 0) cout << a;
        a++;
    }
    
}
