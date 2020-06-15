package com.limitedeternity;

import java.util.Arrays;

class BaseTableEntry {
    public boolean isEmpty() {
        return true;
    }

    public boolean isDeleted() {
        return false;
    }

    public void delete() { }
    public Object getValue() { return null; }
}

class TableEntry<V> extends BaseTableEntry {
    private V value;
    private boolean deleted = false;

    public TableEntry(V val) {
        value = val;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public boolean isDeleted() {
        return deleted;
    }

    @Override
    public void delete() {
        deleted = true;
        value = null;
    }

    @Override
    public V getValue() {
        return value;
    }
}

public class HashTable<V> {
    private int size = 0;
    private int capacity = 5;
    private BaseTableEntry[] table = new BaseTableEntry[capacity];

    public HashTable() {
        Arrays.fill(table, new BaseTableEntry());
    }

    @SuppressWarnings("unchecked")
    private void checkLoadFactor() {
        if (((double) size) / ((double) capacity) > 0.75) {
            BaseTableEntry[] tableCopy = Arrays.copyOf(table, capacity);

            capacity *= 2;
            table = new BaseTableEntry[capacity];
            Arrays.fill(table, new BaseTableEntry());

            for (BaseTableEntry item : tableCopy) {
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
            return (int) value % 101 % capacity;
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
        for (BaseTableEntry item : table) {
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
