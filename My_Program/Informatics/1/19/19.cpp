#include <iostream>
using namespace std;
int main()
{
    int a, b;
    cin >> a >> b;
    cout << (b/a)+((((b/a)*10)/2)%2);
    return 0;
}