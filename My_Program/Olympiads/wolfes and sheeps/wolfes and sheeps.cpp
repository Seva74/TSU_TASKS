#include <iostream>
using namespace std;
int n, m, c;
double t1, t2;
int main()
{
    
    cin >> n >> m;
    //float wx1[n], wy1[n], sx1[m], sy1[m], wx2[n], wy2[n], sx2[m], sy2[m];
    double wx1[1000], wy1[1000], sx1[1000], sy1[1000], wx2[1000], wy2[1000], sx2[1000], sy2[1000];
    for(int i = 1; i <= n; i++) {
        cin >> wx1[i] >> wy1[i] >> wx2[i] >> wy2[i];
    }
    for (int i = 1; i <= m; i++) {
        cin >> sx1[i] >> sy1[i] >> sx2[i] >> sy2[i];
    }
    
    for (int i = 1; i <= n; i++) {
        if (wy1[i] < wy2[i]) {
            swap(wx1[i], wx2[i]);
            swap(wy1[i], wy2[i]);
        }
             
    }
   ;
    for (int i = 1; i <= m; i++) {
        if (sy1[i] <= sy2[i]) {
            swap(sx1[i], sx2[i]);
            swap(sy1[i], sy2[i]);
        }
    }

    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            t1 = (wy1[i] / (wx1[i] / sx2[j]));
            t2 = (wy2[i] / (wx2[i] / sx1[j]));
            //if ((t1 / sy2)>0 || (t1 / sy1)<0 || (t2 / sy2) > 0 || (t2 / sy1) < 0) cout << "No solution";
            if ((((t1 - sy2[j]) > 0 && (t1 - sy1[j]) < 0) || ((t2 - sy2[j]) > 0 && (t2 - sy1[j]) < 0)) == false) {
                cout << "No solution";
                return 0;
            }
               
                
        }
    }
    for (int i = 1; i <= n; ++i)
    {
        for (int j = 1; j <= n; ++j)
        {
            if (i == j) break;
            t1 = (wy1[i] / (wx1[i] / wx2[j]));
            t2 = (wy2[i] / (wx2[i] / wx1[j])); 
            if ((((t1 - wy2[j]) >= 0 && (t1 - wy1[j]) <= 0) || ((t2 - wy2[j]) >= 0 && (t2 - wy1[j]) <= 0)) == false ) c++;

        }
    }
    cout << (c/2);
    return 0;


}
