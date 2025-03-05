#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <ctime>

template <typename T>
class Sort { 
public:
    static int shellSort(T** array, int n) {
        int comparisons = 0;
        for (int gap = n / 2; gap > 0; gap /= 2) {
            for (int i = gap; i < n; i++) {
                T* temp = array[i];
                int j;
                for (j = i; j >= gap; j -= gap) {
                    ++comparisons;
                    if (*array[j - gap] <= *temp) break;
                    array[j] = array[j - gap];
                }
                array[j] = temp;
            }
        }
        return comparisons;
    }

    static int heapSort(T** array, int n) {
        int comparisons = 0;
        for (int i = n / 2 - 1; i >= 0; --i) {
            heapify(array, n, i, comparisons);
        }
        for (int i = n - 1; i >= 0; --i) {
            std::swap(array[0], array[i]);
            heapify(array, i, 0, comparisons);
        }
        return comparisons;
    }

    static void heapify(T** array, int n, int i, int& comparisons) {
        int largest = i;
        int l = 2 * i + 1;
        int r = 2 * i + 2;
        if (l < n && *array[l] > *array[largest]) { 
            ++comparisons;
            largest = l;
        }
        if (r < n && *array[r] > *array[largest]) {
            ++comparisons;
            largest = r;
        }
        if (largest != i) {
            std::swap(array[i], array[largest]);
            heapify(array, n, largest, comparisons);
        }
    }

    static bool isIndirectlySorted(T** array, int n) {
        for (int i = 1; i < n; ++i) {
            if (*array[i - 1] > *array[i]) {
                return false;
            }
        }
        return true;
    }
};

int main() {
    constexpr int n = 1000000;

    std::vector<int*> ascending(n);
    std::vector<int*> descending(n);
    std::vector<int*> random(n);
    for (int i = 0; i < n; ++i) {
        ascending[i] = new int(i);
        descending[i] = new int(n - i - 1);
        random[i] = new int(rand() % n);
    }

    int shellComparisonsAsc = Sort<int>::shellSort(ascending.data(), n);
    int shellComparisonsDesc = Sort<int>::shellSort(descending.data(), n);
    int shellComparisonsRandom = Sort<int>::shellSort(random.data(), n);

    for (int i = 0; i < n; ++i) {
        delete ascending[i];
        delete descending[i];
        delete random[i];
        ascending[i] = new int(i);
        descending[i] = new int(n - i - 1);
        random[i] = new int(rand() % n);
    }

    int heapComparisonsAsc = Sort<int>::heapSort(ascending.data(), n);
    int heapComparisonsDesc = Sort<int>::heapSort(descending.data(), n);
    int heapComparisonsRandom = Sort<int>::heapSort(random.data(), n);

    bool isAscSorted = Sort<int>::isIndirectlySorted(ascending.data(), n);
    bool isDescSorted = Sort<int>::isIndirectlySorted(descending.data(), n);
    bool isRandomSorted = Sort<int>::isIndirectlySorted(random.data(), n);

    std::cout << "Shell sort comparisons (ascending): " << shellComparisonsAsc << std::endl;
    std::cout << "Shell sort comparisons (descending): " << shellComparisonsDesc << std::endl;
    std::cout << "Shell sort comparisons (random): " << shellComparisonsRandom << std::endl;
    std::cout << "Heap sort comparisons (ascending): " << heapComparisonsAsc << std::endl;
    std::cout << "Heap sort comparisons (descending): " << heapComparisonsDesc << std::endl;
    std::cout << "Heap sort comparisons (random): " << heapComparisonsRandom << std::endl;
    std::cout << "Is ascending array indirectly sorted? " << std::boolalpha << isAscSorted << std::endl;
    std::cout << "Is descending array indirectly sorted? " << std::boolalpha << isDescSorted << std::endl;
    std::cout << "Is random array indirectly sorted? " << std::boolalpha << isRandomSorted << std::endl;

    for (size_t i = 0; i < n; ++i) {
        delete ascending[i];
        delete descending[i];
        delete random[i];
    }

    return 0;
}
