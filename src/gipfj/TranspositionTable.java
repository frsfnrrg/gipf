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
    private int collcount;

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
        store = new byte[poolsize][][];
        // store is initialized null
        startsize = 1 << size;
        elemsize = 0;
        poolexp = (32 - pool);

        // I want macros, to optionally turn these guys off
        hitcount = 0;
        misscount = 0;
        collcount = 0;
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
            index = -2 * index - 1;
        } else {
            index = 2 * index;
        }

        byte[][] bucket = store[index];
        if (bucket == null) {
            misscount++;
            return null;
        }

        byte[] a = in.getData();
        for (byte[] elem : bucket) {
            if (elem == null) {
                break;
            }

            if (a.length != elem.length) {
                continue;
            }

            boolean c = false;
            // we skip the first 4 (0,1,2,3) as these hold the rank
            for (int i = 4; i < a.length; i++) {
                if (a[i] != elem[i]) {
                    c = true;
                    collcount++;
                    break;
                }
            }

            if (c) {
                continue;
            } else {
                // it matches
                hitcount++;
                return (long) ((0xFF & elem[0]) << 24)
                        | ((0xFF & elem[1]) << 16) | ((0xFF & elem[2]) << 8)
                        | (0xFF & elem[3]);
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
    public void change(Compressed in, int rank) {

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
    public void add(Compressed in, int rank) {
        int index = in.hashCode() >> poolexp;
        if (index < 0) {
            index = -2 * index - 1;
        } else {
            index = 2 * index;
        }

        byte[] dat = in.getData();
        dat[0] = (byte) (rank >> 24);
        dat[1] = (byte) (rank >> 16);
        dat[2] = (byte) (rank >> 8);
        dat[3] = (byte) (rank >> 0);

        byte[][] bucket = store[index];
        if (bucket == null) {
            bucket = new byte[startsize][];
            bucket[0] = dat;
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
                byte[][] n = new byte[size * 2][];
                System.arraycopy(bucket, 0, n, 0, size);
                n[size] = dat;
                store[index] = n;
            } else {
                for (int i = size - 2; i >= 0; i--) {
                    if (bucket[i] != null) {
                        bucket[i + 1] = dat;
                        break;
                    }
                }
            }
        }
        elemsize++;
    }

    public void clear() {
        store = new byte[poolsize][][];
        elemsize = 0;

        double hr = 100.0 * hitcount / (hitcount + misscount);
        double cr = 100.0 * collcount / (hitcount + misscount);

        // at poolexp 21, 938K items, we have a 22.3% collision rate..
        // Woah.

        System.out
                .format("Clearing. Hits: %d. Misses: %d. Collisions: %d. Hit rate: %f %%. Collision rate: %f %%\n",
                        hitcount, misscount, collcount, hr, cr);
        hitcount = 0;
        misscount = 0;
        collcount = 0;

        // let GC do its work.
        System.gc();
    }

    public void analyze() {
        // gather stats about the

    }

    private final int startsize;

    private byte[][][] store;

    // STATIC ACCESS METHODS

    public static Long tsize(TranspositionTable t) {
        return t.getSize();
    }

    private static int memcnt = 0;
    private static boolean lockdown = false;

    // these two constants should be chosen so that, MCI additions
    // will not pass the MDT, within a nice safety factor.
    public static final int MEMORY_DANGER_THRESHOLD = (1 << 24);
    public static final int MEMORY_CHECK_INTERVAL = 10000;

    public static void tadd(TranspositionTable t, Compressed in, long rank) {
        memcnt++;
        if (memcnt == MEMORY_CHECK_INTERVAL) {
            memcnt = 0;

            if (lockdown) {
                System.out.println("*** Checking memory situation ***");
            }

            if ((Runtime.getRuntime().maxMemory()
                    - Runtime.getRuntime().totalMemory() < MEMORY_DANGER_THRESHOLD)
                    && Runtime.getRuntime().freeMemory() < MEMORY_DANGER_THRESHOLD) {
                System.out
                        .format("***WARNING*** OOM APPROACHING!!! free %d : total %d : max %d\n",
                                Runtime.getRuntime().freeMemory(), Runtime
                                        .getRuntime().totalMemory(), Runtime
                                        .getRuntime().maxMemory());
                lockdown = true;
                return;
            } else {
                lockdown = false;
            }
        }

        if (lockdown == false) {
            t.add(in, (int) rank);
        }
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