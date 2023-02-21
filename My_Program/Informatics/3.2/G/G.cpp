#include <iostream>

using namespace std;
int x;
int main()
{
    cin >> x;
    for (int i = 2; i <= x; i++) {
        if (x % i == 0) {
            cout << i;
            i = x + 1;
        }

    }
}
