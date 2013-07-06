package gipfj;

/**
 * WHY? Because allocating 350M entries is a bit too .... stressful.
 * 
 * 
 */
public class EntryPool {
    private Entry head;
    public int size;
    public int returned;

    public EntryPool() {
        head = null;
        size = 0;
        returned = 0;
    }

    /**
     * This detaches the entire chain of entries.
     * 
     */
    public void flush() {
        head = null;
        size = 0;
        returned = 0;
    }

    public int getSize() {
        int k = 0;
        Entry q = head;
        while (q != null && k < Integer.MAX_VALUE) {
            q = q.second;
            k++;
        }
        return k;
    }

    /**
     * User must pass in an entry; NOT null;
     * 
     * @param e
     */
    public void returnEntry(Entry e) {
        e.second = head;
        head = e;
        size++;
        returned++;
    }

    /**
     * Warning: rank and depth fields may retain old data;
     * 
     * @param data
     * @param hc
     * @return
     */
    public Entry getEntry(long a, int b, int hc) {
        if (head == null) {
            return new Entry(a, b, hc);
        }
        Entry e = head;
        head = e.second;
        size--;
        e.reset(a, b, hc);
        return e;
    }
}
