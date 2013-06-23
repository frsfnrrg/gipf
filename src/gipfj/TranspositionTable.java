package gipfj;

import java.util.HashMap;

/**
 * Maps objects to integers. Supports get, put, change, clear, and size.
 * 
 */
public class TranspositionTable {
    private int elemsize;
    private final int poolexp;
    private final int poolsize;
    private int hitcount;
    private int misscount;

    /**
     * Pool is the exponent for pool size; pool 8 -> 64 size Size is the
     * exponent for bucket size: size 2 -> 8 bucket
     * 
     * Larger pool - more memory at first, better response.
     * 
     * Larger buckets - more memory, less bucket expansion on inevitable
     * collisions.
     * 
     */
    public TranspositionTable(int pool, int size) {
        poolsize = 1 << pool;
        store = new KVPair[poolsize][];
        // store is initialized null
        startsize = 1 << size;
        elemsize = 0;
        poolexp = (32 - pool) + 1;
        hitcount = 0;
        misscount = 0;
    }

    /**
     * Long : clojure. Meh.
     * 
     * @return
     */
    public Long getSize() {
        return (long) elemsize;
    }

    // is there any way to prevent repeating the
    // get: looks it up ; returns
    // set: look it up ; changes
    // pattern?
    /**
     * This returns a Long for better compatibility with Clojure. Also, Longs
     * can be null.
     * 
     * @param in
     * @return
     */
    public Long get(Compressed in) {
        int index = in.hashCode() >> poolexp;
        if (index < 0) {
            index = -2 * index + 1;
        } else {
            index = 2 * index;
        }

        KVPair[] bucket = store[index];
        if (bucket == null) {
            // System.out.format("nullbucket @%d\n", index);
            misscount++;
            return null;
        }
        for (KVPair elem : bucket) {
            if (elem == null) {
                break;
            }

            byte[] a = in.getData();
            byte[] b = elem.data;

            if (a.length != b.length) {
                continue;
            }

            boolean c = false;
            for (int i = 0; i < a.length; i++) {
                if (a[i] != b[i]) {
                    c = true;
                    break;
                }
            }

            if (c) {
                continue;
            } else {
                // it matches
                hitcount++;
                return (long) elem.rank;
            }
        }
        misscount++;
        return null;
    }

    /**
     * NOT IMPLEMENTED YET
     * 
     * @param in
     * @param rank
     */
    public void change(Compressed in, long rank) {

    }

    /**
     * Reads an int from the first four bytes of data.
     * 
     * @param data
     * @return
     */
    private int bytesToInt(byte[] data) {

        return ((0xFF & data[0]) << 24) | ((0xFF & data[1]) << 16)
                | ((0xFF & data[2]) << 8) | (0xFF & data[3]);
    }

    /**
     * Changes the first four bytes of data to the value of the int.
     * 
     * @param data
     * @return
     */
    private void intToBytes(byte[] data, int g) {
        // [sign b7 b6 b5 b4 b3 b2 b1] []

        data[0] = (byte) (g >> 24);
        data[1] = (byte) (g >> 16);
        data[2] = (byte) (g >> 8);
        data[3] = (byte) (g >> 0);
    }

    /**
     * Rank is a long, again for Clojure code compatibility.
     * 
     * Warning: the object should not already be in the Transposition Table. In
     * that case, use <code>change(T in, long new)</code>;
     * 
     * @param in
     * @param rank
     */
    public void add(Compressed in, long rank) {
        int index = in.hashCode() >> poolexp;
        if (index < 0) {
            index = -2 * index + 1;
        } else {
            index = 2 * index;
        }

        KVPair[] bucket = store[index];
        if (bucket == null) {
            // System.out.format("creating @%d\n", index);
            bucket = new KVPair[startsize];
            bucket[0] = new KVPair(in, (int) rank);
            store[index] = bucket;
        } else {
            // we could make additions cheaper by changing KVPair[] to
            // a new type, which records the last position. Not worth it
            // until profiling..

            // search backwards - the first half is already full, no
            // need to continue iterating. Or search forwards from halfway point
            // - faster, given expected log increase in size
            int size = bucket.length;
            if (bucket[size - 1] != null) {
                KVPair[] n = new KVPair[size * 2];
                System.arraycopy(bucket, 0, n, 0, size);
                n[size] = new KVPair(in, (int) rank);
                store[index] = n;
                // System.out.format("expanding @%d\n", index);
                // expand
            } else {
                for (int i = size - 2; i >= 0; i--) {
                    if (bucket[i] != null) {
                        bucket[i + 1] = new KVPair(in, (int) rank);
                        break;
                    }
                }
            }
        }
        elemsize++;
    }

    public void clear() {
        store = new KVPair[poolsize][];
        elemsize = 0;

        double ratio = 100.0 * hitcount / (hitcount + misscount);

        System.out.format(
                "Clearing. Hits: %d. Misses: %d. Hit percentage: %f %%\n",
                hitcount, misscount, ratio);
        hitcount = 0;
        misscount = 0;
        // let gc do its work.
    }

    private final int startsize;

    private static class KVPair {
        public KVPair(Compressed item, int rank) {
            this.data = item.getData();
            this.rank = rank;
        }

        public final byte[] data;
        public int rank;
    }

    private KVPair[][] store;

    // STATIC ACCESS METHODS

    public static Long tsize(TranspositionTable t) {
        return t.getSize();
    }

    public static void tadd(TranspositionTable t, Compressed in, long rank) {
        t.add(in, rank);
    }

    public static Object tget(TranspositionTable t, Compressed in) {
        return t.get(in);
    }

    public static void tclear(TranspositionTable t) {
        t.clear();
    }

    // HASHMAP METHODS: THIS SUCKS MEMORY..

    public static HashMap<SignedGameState, Integer> makeTranspTable(int size) {
        return new HashMap<SignedGameState, Integer>(size);
    }

    public static Long getVal(HashMap<SignedGameState, Integer> t,
            SignedGameState i) {
        Integer r = t.get(i);
        if (r == null) {
            return null;
        } else {
            return r.longValue();
        }
    }

    public static void addKeyVal(HashMap<SignedGameState, Integer> t,
            SignedGameState a, Long b) {
        t.put(a, b.intValue());
    }

    public static void flush(HashMap<SignedGameState, Integer> t) {
        t.clear();
    }

    public static Long count(HashMap<SignedGameState, Integer> t) {
        return (long) t.size();
    }
}