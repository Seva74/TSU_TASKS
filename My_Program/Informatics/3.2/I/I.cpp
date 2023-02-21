#include <iostream>

using namespace std;
int x,a;
int main()
{
    cin >> x;
    for (int i = 1; i <= 2000000000; i++) {
        if (x % i == 0) a++;

    }
    cout << a;
}