#include <iostream>

using namespace std;

int i,n;
int arr[1000];
unsigned char packed[1000];

int pack(int n) {
    for(i=0;i<n;i++) packed[i] = arr[i];
}

int unpack(int n) {
    for(i=0;i<n;i++) arr[i] = packed[i];
}

int main() {
    cout << "Enter the length of the array: ";
    cin >> n;
    cout << "Enter array elements: ";

    for (int i = 0; i < n; i++) {
        arr[i] = rand()%18;
    }
    pack(n);
    unpack(n);
    cout << "Unpacked array: ";
    for (int i = 0; i < n; i++) {
        cout << arr[i] << " ";
    }
}
