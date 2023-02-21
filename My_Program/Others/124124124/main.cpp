#include <iostream>

using namespace std;
int main()
{
    for(int x=2;x<=1000;x++)
    {
        for(int y=2;y<=1000;y++)
    {
        if( (2*y*y-2*y) == (x*x-x)   )
        {
            cout<<" y = "<< y<< " x = "<< x <<endl;
            break;
        }

    }

    }
}
