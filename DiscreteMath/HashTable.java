package com.limitedeternity;

import java.util.Arrays;

enum EntryStates {
    EMPTY,
    DELETED,
    HAS_DATA
}

class TableEntry<V> {
    private V value;
    private EntryStates state;

    public TableEntry(V val) {
        value = val;
        state = EntryStates.HAS_DATA;
    }

    public TableEntry() {
        value = null;
        state = EntryStates.EMPTY;
    }

    public boolean isEmpty() {
        return state.equals(EntryStates.EMPTY);
    }

    public boolean isDeleted() {
        return state.equals(EntryStates.DELETED);
    }

    public void delete() {
        value = null;
        state = EntryStates.DELETED;
    }

    public V getValue() {
        return value;
    }
}

public class HashTable<V> {
    private int size = 0;
    private int capacity = 5;
    private TableEntry<?>[] table = new TableEntry[capacity];

    public HashTable() {
        Arrays.fill(table, new TableEntry<V>());
    }

    @SuppressWarnings("unchecked")
    private void checkLoadFactor() {
        if (((double) size) / ((double) capacity) > 0.75) {
            TableEntry<?>[] tableCopy = Arrays.copyOf(table, capacity);

            capacity *= 2;
            table = new TableEntry[capacity];
            Arrays.fill(table, new TableEntry<V>());

            for (TableEntry<?> item : tableCopy) {
                if (!item.isEmpty() && !item.isDeleted()) {
                    V value = (V) item.getValue();
                    TableEntry<V> entry = new TableEntry<>(value);

                    int key = hashFunction(value);
                    while (!table[key].isEmpty()) {
                        key = (key + 1) % capacity;
                    }

                    table[key] = entry;
                }
            }
        }
    }

    private int hashFunction(V value) {
        if (value.getClass().isAssignableFrom(Integer.class)) {
            return (Integer) value % 101 % capacity;
        } else {
            return value.hashCode() % capacity;
        }
    }

    public boolean contains(V value) {
        int key = hashFunction(value);
        while (!table[key].isEmpty()) {
            if (!table[key].isDeleted() && table[key].getValue().equals(value)) {
                return true;
            }

            key = (key + 1) % capacity;
        }

        return false;
    }

    public void delete(V value) {
        int key = hashFunction(value);
        while (!table[key].isEmpty()) {
            if (table[key].getValue().equals(value)) {
                table[key].delete();
                return;
            }

            key = (key + 1) % capacity;
        }
    }

    public void put(V value) {
        TableEntry<V> entry = new TableEntry<>(value);
        int key = hashFunction(value);
        while (!table[key].isEmpty()) {
            if (!table[key].isDeleted() && table[key].getValue().equals(value)) {
                return;
            }

            key = (key + 1) % capacity;
        }

        table[key] = entry;
        size++;
        checkLoadFactor();
    }

    @Override
    public String toString() {
        StringBuilder tableStringBuilder = new StringBuilder();
        int i = 0;
        for (TableEntry<?> item : table) {
            if (item.isEmpty()) {
                tableStringBuilder.append(i).append(":= []");
            } else if (item.isDeleted()) {
                tableStringBuilder.append(i).append(":= [X]");
            } else {
                tableStringBuilder.append(i).append(":= ").append(item.getValue());
            }

            i++;
            if (i < capacity) {
                tableStringBuilder.append("\n");
            }
        }

        return tableStringBuilder.toString();
    }
}
