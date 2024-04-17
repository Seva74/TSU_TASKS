#include <iostream>
#include <cstring>

// Класс SList - список уникальных строк
class SList {
private:
    struct Node {
        char* data;
        Node* next;
        Node(char* data) : data(data), next(nullptr) {}
    };

    Node* head;

public:
    SList() : head(nullptr) {}

    bool contains(char* str) {
        Node* current = head;
        while (current != nullptr) {
            if (strcmp(current->data, str) == 0) {
                return true;
            }
            current = current->next;
        }
        return false;
    }

    void add(char* str) {
        if (!contains(str)) {
            Node* newNode = new Node(str);
            newNode->next = head;
            head = newNode;
        }
    }

    void merge(SList& other) {
        Node* current = other.head;
        while (current != nullptr) {
            add(current->data);
            current = current->next;
        }
    }

    int getCount() {
        int count = 0;
        Node* current = head;
        while (current != nullptr) {
            count++;
            current = current->next;
        }
        return count;
    }

    ~SList() {
        Node* current = head;
        while (current != nullptr) {
            Node* temp = current;
            current = current->next;
            delete[] temp->data;
            delete temp;
        }
    }
};

// Класс HashTable - хеш-таблица с методом цепочек
class HashTable {
private:
    int q;
    SList* table;

    int hash(char* str) {
        int hashValue = 0;
        int len = strlen(str);
        for (int i = 0; i < len; i++) {
            hashValue = (hashValue * 31 + str[i]) % q;
        }
        return hashValue;
    }

public:
    HashTable(int q) : q(q) {
        table = new SList[q];
    }

    ~HashTable() {
        delete[] table;
    }

    void add(char* str) {
        int index = hash(str);
        table[index].add(str);
    }

    void mergeAll(SList& uniqueStrings) {
        for (int i = 0; i < q; i++) {
            uniqueStrings.merge(table[i]);
        }
    }
};

int main() {
    int n, range, q;
    std::cout << "Enter number of strings (n <= 100000): ";
    std::cin >> n;
    std::cout << "Enter range of characters: ";
    std::cin >> range;
    std::cout << "Enter size of hash table (q): ";
    std::cin >> q;

    char** strings = new char* [n];
    HashTable hashTable(q);

    // Генерация исходного массива строк
    for (int i = 0; i < n; i++) {
        int len = rand() % range + 1;
        strings[i] = new char[len + 1];
        for (int j = 0; j < len; j++) {
            strings[i][j] = 'a' + rand() % 26; // Случайный символ от 'a' до 'z'
        }
        strings[i][len] = '\0';
        hashTable.add(strings[i]);
    }
    
    SList uniqueStrings;
    hashTable.mergeAll(uniqueStrings);

    std::cout << "Total comparisons: " << n << std::endl;
    std::cout << "Number of unique strings: " << uniqueStrings.getCount() << std::endl;
    
    // Освобождение памяти
    for (int i = 0; i < n; i++) {
        delete[] strings[i];
    }
    delete[] strings;
    
    exit(0);
    return 0;
}
