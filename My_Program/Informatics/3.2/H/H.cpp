#include <iostream>

using namespace std;
int x;
int main()
{
    cin >> x;
    for (int i = 1; i <= x; i++) {
        if (x % i == 0) cout << i << " ";
    }
}