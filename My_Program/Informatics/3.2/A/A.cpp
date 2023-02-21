#include <iostream>

using namespace std;
int a, b;
int main()
{
    cin >> a >> b;
    if (a % 2 == 0) {


        for (int i = a; i <= b; i += 2) {
            cout << i << " ";
        }
    }
    else {
        a++;
        for (int i = a; i <= b; i += 2) {
            cout << i << " ";
        }
    }
    return 0;
}
