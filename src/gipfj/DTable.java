package gipfj;

/**
 * A depth two transposition table with depth-based replacement scheme - new
 * nodes are always accepted, and nodes that were searched deeper replace
 * short-depth nodes. (this idea comes from <i>Replacement Schemes for
 * Transposition Tables</i>)
 * 
 * NOTE: using the public synchronized whatever() strategy is slow.
 * 
 */
public class DTable {
    private final int size;
    private final int shift_cut;
    private int count_null;
    private int count_first;
    private int count_second;
    private int count_csecond;
    private int count_cneither;
    private int count_cfirst;
    private int count_cnull;
    private int pushouts;

    private DTable(int sexp) {
        // 48 bytes/elem; 4 bytes at reference; "approximation" of overhead.
        long max_size = (1 << sexp) * 100;
        long omax = max_size;
        int osexp = sexp;
        Runtime foo = Runtime.getRuntime();
        foo.gc();
        long safe_size = foo.maxMemory() - foo.totalMemory() + foo.freeMemory()
                - (1 << 26); // safety margin
        boolean c = false;
        while (safe_size < max_size && sexp >= 5) {
            c = true;
            sexp--;
            max_size = (1 << sexp) * 100;
        }
        if (c) {
            System.out
                    .format("&& DTable init warning: when full, table would have used %d bytes.\n",
                            omax);
            System.out.format(
                    "&& Estimated free bytes: %d; Overflow (bytes): %d\n",
                    safe_size, omax - safe_size);
            System.out.format("&& Size exponent reduced from %d to %d\n",
                    osexp, sexp);
            System.out.format("&& Free memory remaining: %d bytes\n", safe_size
                    - max_size);
        }

        size = 1 << sexp;
        shift_cut = 32 - sexp;
        store = new Entry[size];

        count_null = 0;
        count_first = 0;
        count_second = 0;

        count_cnull = 0;
        count_cfirst = 0;
        count_csecond = 0;
        count_cneither = 0;

        pushouts = 0;
    }

    private Entry[] store;

    private void add(ThreadBuffer buf, Entry n, int depth, int rank) {
        if (store == null) {
            store = new Entry[size];
        }

        int index = n.hc >>> shift_cut;

        n.depth = (byte) depth;
        n.rank = rank;

        Entry ff = store[index];
        if (ff == null) {
            store[index] = n;
        } else {
            // first entry always has greater depth;
            // second entry is pushed out
            if (ff.second != null) {
                buf.EPOOL.returnEntry(ff.second);
            }

            if (n.depth > ff.depth) {
                store[index] = n;
                n.second = ff;
                ff.second = null;
                pushouts++;
            } else {
                ff.second = n;
            }
        }
    }

    /**
     * This does not do a memory check. You might as well make a new instance.
     * 
     */
    private void load() {
        if (store == null) {
            store = new Entry[size];
        }
    }

    private void empty() {
        count_null = 0;
        count_first = 0;
        count_second = 0;
        count_cnull = 0;
        count_cfirst = 0;
        count_csecond = 0;
        count_cneither = 0;
        Entry.allocated = 0;
        pushouts = 0;
        store = null;
        System.gc();
    }

    private void analyze() {
        int empty = 0;
        int single = 0;
        int full = 0;

        for (Entry e : store) {
            if (e == null) {
                empty++;
            } else if (e.second == null) {
                single++;
            } else {
                full++;
            }
        }
        System.out.println("== Depth 2 table analysis results:");
        System.out.format("== Search: fail - %d; first %d; second %d\n",
                count_null, count_first, count_second);
        System.out.format(
                "== Change: empty - %d; first %d; second %d; neither %d\n",
                count_cnull, count_cfirst, count_csecond, count_cneither);
        System.out.format("== State: empty %d; single entry %d; full %d\n",
                empty, single, full);
        System.out.format("== Allocated: %d; Resident: %d; Pushouts: %d;\n",
                Entry.allocated, single + 2 * full, pushouts);

    }

    private Long geta(Entry in) {
        int index = in.hc >>> shift_cut;
        Entry f = store[index];
        if (f == null) {
            count_null++;
            return null;
        }

        if (f.equals(in)) {
            count_first++;
            return (long) f.rank;
        } else if (f.second != null && f.second.equals(in)) {
            count_second++;
            return (long) f.second.rank;
        }
        return null;
    }

    private Long getd(Entry n, byte depth) {
        int index = n.hc >>> shift_cut;

        Entry f = store[index];
        if (f == null) {
            count_null++;
            return null;
        }

        if (f.equals(n) && f.depth == depth) {
            count_first++;
            return (long) f.rank;
        } else if (f.second != null && f.second.equals(n)
                && f.second.depth == depth) {
            count_second++;
            return (long) f.second.rank;
        }
        return null;
    }

    /**
     * This should be threadsafe - changing a reference, as done in add and
     * change, is atomic.
     * 
     * @param buf
     * @param n
     * @param depth
     * @param rank
     */
    private void change(ThreadBuffer buf, Entry n, byte depth, int rank) {
        int index = n.hc >>> shift_cut;

        Entry ff = store[index];
        if (ff == null) {
            count_cnull++;
            add(buf, n, depth, rank);
            return;
        }

        if (ff.equals(n)) {
            n.second = ff.second;
            n.rank = rank;
            n.depth = depth;
            store[index] = n;
            count_cfirst++;
            buf.EPOOL.returnEntry(ff);
        } else if (ff.second != null && ff.second.equals(n)) {
            buf.EPOOL.returnEntry(ff.second);
            count_csecond++;
            n.second = null;
            n.rank = rank;
            n.depth = depth;
            ff.second = n;
        } else {
            count_cneither++;
            add(buf, n, depth, rank);
        }
    }

    public static DTable dmake(long size) {
        return new DTable((int) size);
    }

    public static void dadd(DTable d, ThreadBuffer b, Entry gs, long depth,
            long rank) {
        d.add(b, gs, (int) depth, (int) rank);
    }

    /**
     * Depth need not match.
     * 
     * @param d
     * @param gs
     * @return
     */
    public static Long dgeta(DTable d, Entry gs) {
        return d.geta(gs);
    }

    /**
     * Depth _must_ match.
     * 
     * @param d
     * @param gs
     * @param depth
     * @return
     */
    public static Long dgetd(DTable d, Entry gs, long depth) {
        return d.getd(gs, (byte) depth);
    }

    /**
     * Depth need not match;
     * 
     * @param d
     * @param gs
     * @param depth
     * @param rank
     */
    public static void dchange(DTable d, ThreadBuffer b, Entry gs, long depth,
            long rank) {
        d.change(b, gs, (byte) depth, (int) rank);
    }

    public static void dclear(DTable d) {
        d.empty();
        d.load();
    }

    public static void dempty(DTable d) {
        d.empty();
    }

    public static void dload(DTable d) {
        d.load();
    }

    public static void danalyze(DTable d) {
        d.analyze();
    }
}
