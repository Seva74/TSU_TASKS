#include <iostream>
#include <string>
using namespace std;

int main(){
    int n;
    string words[20];

    cout << "Enter the number of words no more than 20"<<endl;
    cin >> n;
    cout << "Enter "<<n<<" words which length is less than 10 characters"<<endl;
    for (int i = 0; i < n; i++) {
        cout<< i + 1 << ": ";
        cin >> words[i];
    }
    cout << "Words with even numbers:" << endl;
    for (int i = 1; i < n; i += 2) {
        cout << words[i] << endl;
    }

}
