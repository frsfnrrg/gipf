package gipfj;

/**
 * A clone of the TranspositionTable, with another layer of data-saving.
 * 
 * This is designed to use as little memory as possible - even at the cost of a
 * new ArrayCopy on collision.
 * 
 * [H:A:S:H|R:A:N:K|D:A:T:A:.:.: ... .:.:|H:A:S:H| ...
 * 
 * 
 * @author msto
 * 
 */
public class LTranspTable {
    private int elemsize;
    private final int poolexp;
    private final int poolsize;
    private int hitcount;
    private int misscount;
    private int collcount;
    private final int datasize;
    private byte[][] store;

    /**
     * Pool is the exponent for pool size; pool 8 -> 64 size Size is the
     * exponent for bucket size: size 2 -> 8 bucket
     * 
     * Larger pool - more memory at first, better response.
     * 
     * Larger buckets - more memory, less bucket expansion on inevitable
     * collisions.
     * 
     * Data length is the length of the used data in a compressed object. It
     * includes the space for rank
     */
    public LTranspTable(int pool, int datalength) {
        poolsize = 1 << pool;
        store = new byte[poolsize][];
        // store is initialized null
        datasize = datalength;
        elemsize = 0;
        poolexp = (32 - pool);

        // I want macros, to optionally turn these guys off
        hitcount = 0;
        misscount = 0;
        collcount = 0;
    }

    public Long getSize() {
        return (long) elemsize;
    }

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

        byte[] bucket = store[index];
        if (bucket == null) {
            misscount++;
            return null;
        }

        byte[] a = in.getData();
        for (int i = 4; i < bucket.length; i += datasize) {
            boolean c = false;
            for (int j = 4; j < datasize; j++) {
                if (a[j] != bucket[j + i]) {
                    c = true;
                    collcount++;
                    break;
                }
            }

            if (c) {
                continue;
            } else {
                hitcount++;
                return (long) ((0xFF & bucket[i]) << 24)
                        | ((0xFF & bucket[i + 1]) << 16)
                        | ((0xFF & bucket[i + 2]) << 8)
                        | (0xFF & bucket[i + 3]);
            }

        }

        misscount++;
        return null;
    }

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
        int hc = in.hashCode();
        int index = hc >> poolexp;
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

        byte[] bucket = store[index];
        if (bucket == null) {
            bucket = new byte[4 + datasize];
            // we store the HashCode for rehashing/expansion purposes
            // (if we ever implement it).
            // At minimum, it is only 4 bytes - we could save 12 on
            // compressing better alone.
            bucket[0] = (byte) (hc >> 24);
            bucket[1] = (byte) (hc >> 16);
            bucket[2] = (byte) (hc >> 8);
            bucket[3] = (byte) (hc >> 0);

            System.arraycopy(dat, 0, bucket, 4, datasize);

            store[index] = bucket;
        } else {
            // expand the bucket..
            int size = bucket.length;
            byte[] o = bucket;
            byte[] n = new byte[size + datasize];
            System.arraycopy(o, 0, n, 0, size);
            System.arraycopy(dat, 0, n, size, datasize);
        }

        elemsize++;
    }

    public void clear() {
        store = new byte[poolsize][];
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

    public static LTranspTable tmake(int pool) {
        // as per compressedSGS sig
        return new LTranspTable(pool, 4 + 27);
    }

    public static Long tsize(LTranspTable t) {
        return t.getSize();
    }

    private static int memcnt = 0;
    private static boolean lockdown = false;

    // these two constants should be chosen so that, MCI additions
    // will not pass the MDT, within a nice safety factor.
    public static final int MEMORY_DANGER_THRESHOLD = (1 << 24);
    public static final int MEMORY_CHECK_INTERVAL = 10000;

    public static void tadd(LTranspTable t, CompressedSGS in, long rank) {
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

    public static Object tget(LTranspTable t, CompressedSGS in) {
        return t.get(in);
    }

    public static void tclear(LTranspTable t) {
        t.clear();
    }

}
