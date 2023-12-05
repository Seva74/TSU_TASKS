#include <iostream>

using namespace std;

int i, n;
int arr[1000];
unsigned char packed[1000];

int pack(int n) {
    int count = 1;
    int j = 0;
    for (i = 1; i < n; i++) {
        if (arr[i] != arr[i-1]) {
            packed[j++] = count;
            packed[j++] = arr[i-1];
            count = 1;
        } else {
            count++;
        }
    }
    packed[j++] = count;
    packed[j++] = arr[n-1];
    return j;
}

int unpack(int n) {
    int j = 0;
    for (i = 0; i < n; i+=2) {
        for (int k = 0; k < packed[i]; k++) {
            arr[j++] = packed[i+1];
        }
    }
}

int main() {
    cout << "Enter the length of the array: ";
    cin >> n;
    cout<<"source array: ";
    for (int i = 0; i < n; i++) {
        arr[i] = rand() % 18;
        cout<<arr[i]<<" ";
    }

    int packed_size = pack(n);
    cout <<endl<< "Packed array: ";
    for (int i = 0; i < packed_size; i++) {
        cout << int(packed[i]) << " ";
    }
    cout << endl;

    unpack(packed_size);
    cout << "Unpacked array: ";
    for (int i = 0; i < n; i++) {
        cout << arr[i] << " ";
    }
    cout << endl;

    return 0;
}
