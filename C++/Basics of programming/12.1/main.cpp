#include <iostream>
#include <vector>

using namespace std;


// generation of all permutations
void generatePermutations(vector<int>& arr, int start, int end) {
    if (start != end) {
        // Generate all permutations
        for (int i = start; i <= end; i++) {
        // Swap the current element with the element at position start
        swap(arr[start], arr[i]);
        // Generate permutations for the rest of the array
        generatePermutations(arr, start + 1, end);
        // Return the initial state of the array
        swap(arr[start], arr[i]);
        }
    }
     else {
        // Outputting the current permutation
        for (int i = 0; i <= end; i++) cout << arr[i] << " ";
        cout << endl;
}
}

int main() {
    int n;
    cout << "Enter the number of elements: ";
    cin >> n;
    vector<int> arr(n);
    cout << "Enter the elements:"<<endl;
    for (int i = 0; i < n; i++) {
        cin >> arr[i];
    }
    cout << "Permutations: " << endl;
    generatePermutations(arr, 0, n - 1);
    return 0;
}
