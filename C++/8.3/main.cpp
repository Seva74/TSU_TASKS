#include <iostream>

using namespace std;

void fillArrayStaticIndex(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        arr[i] = i * i;
    }
}

void fillArrayStaticPointer(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        *(arr + i) = i * i;
    }
}

void fillArrayDynamicIndex(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        arr[i] = i * i;
    }
}

void fillArrayDynamicPointer(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        *(arr + i) = i * i;
    }
}

int main() {
    const int size = 10;

    int arrStaticIndex[size];
    fillArrayStaticIndex(arrStaticIndex, size);
    cout << "Static Array with Index Addressing: ";
    for (int i = 0; i < size; i++) {
        cout << arrStaticIndex[i] << " ";
    }
    cout << endl;

    int arrStaticPointer[size];
    fillArrayStaticPointer(arrStaticPointer, size);
    cout << "Static Array with Pointer Addressing: ";
    for (int i = 0; i < size; i++) {
        cout << *(arrStaticPointer + i) << " ";
    }
    cout << endl;

    int* arrDynamicIndex = new int[size];
    fillArrayDynamicIndex(arrDynamicIndex, size);
    cout << "Dynamic Array with Index Addressing: ";
    for (int i = 0; i < size; i++) {
        cout << arrDynamicIndex[i] << " ";
    }
    cout << endl;
    delete[] arrDynamicIndex;

    int* arrDynamicPointer = new int[size];
    fillArrayDynamicPointer(arrDynamicPointer, size);
    cout << "Dynamic Array with Pointer Addressing: ";
    for (int i = 0; i < size; i++) {
        cout << *(arrDynamicPointer + i) << " ";
    }
    cout << endl;
    delete[] arrDynamicPointer;

    return 0;
}
