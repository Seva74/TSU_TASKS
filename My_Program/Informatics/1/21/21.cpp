#include <iostream>
using namespace std;
int main()
{
    int a, b;
    cin >> a >> b;
    cout << (a * (a / b) + b * (b / a)) / (b / a + a / b);
    return 0;
}  
