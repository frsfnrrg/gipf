package gipfj;

import java.lang.reflect.Field;

import sun.misc.Unsafe;

/**
 * Note: the vast majority of the execution time is in loading things from DRAM.
 * 
 * e.g. calling get is 788 ns
 * 
 * 
 */
public class STable {
    private static final Unsafe unsafe = getTheUnsafe();
    private static long valueOffset;

    private static Unsafe getTheUnsafe() {
        try {
            Field singleoneInstanceField = Unsafe.class
                    .getDeclaredField("theUnsafe");
            singleoneInstanceField.setAccessible(true);
            return (Unsafe) singleoneInstanceField.get(null);

        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }

        return null;
    }

    static {
        try {
            valueOffset = unsafe.objectFieldOffset(Block.class
                    .getDeclaredField("lock"));
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        } catch (SecurityException e) {
            e.printStackTrace();
        }
    }

    private final Block[] store;
    private final int shift_cut;
    private final int size;
    private static int collisions;
    private static int hit_first;
    private static int hit_second;
    private static int failed;

    public STable(int sz) {
        size = sz;
        int dim = 1 << size;
        shift_cut = 32 - size;
        // flush
        store = new Block[dim];
        for (int i = 0; i < dim; i++) {
            store[i] = new Block();
        }
        collisions = 0;
        hit_first = 0;
        hit_second = 0;
        failed = 0;
    }

    public void flush() {
        for (Block b : store) {
            b.f_open = false;
            b.s_open = false;
        }
        collisions = 0;
        hit_first = 0;
        hit_second = 0;
        failed = 0;
    }

    public void analyze() {
        System.out.println("^^ Transpostion table analysis results:");
        System.out.format("^^ Colls: %d First: %d Second: %d Failed: %d \n",
                collisions, hit_first, hit_second, failed);
        int empty = 0;
        int single = 0;
        int paired = 0;
        for (Block b : store) {
            if (!b.f_open) {
                empty++;
                continue;
            }
            if (b.s_open) {
                paired++;
            } else {
                single++;
            }
        }
        System.out.format("^^ Empty: %d Single: %d Paired %d\n", empty, single,
                paired);
    }

    public void add(Ident in, byte depth, int rank) {
        int index = in.hc >>> shift_cut;
        store[index].add(in, depth, rank);
    }

    public Long get(Ident in, byte depth) {
        int index = in.hc >>> shift_cut;
        return store[index].get(in, depth);
    }

    public void update(Ident in, byte depth, int rank) {
        int index = in.hc >>> shift_cut;
        store[index].update(in, depth, rank);
    }

    public static void sadd(STable s, Ident in, long depth, long rank) {
        s.add(in, (byte) depth, (int) rank);
    }

    public static Long sget(STable s, Ident in, long depth) {
        return s.get(in, (byte) depth);
    }

    public static void supdate(STable s, Ident in, long depth, long rank) {
        s.update(in, (byte) depth, (int) rank);
    }

    public static void sanalyze(STable s) {
        s.analyze();
    }

    public static void sempty(STable s) {
        s.flush();
    }

    private static class Block {
        // it doesn't recognize unsafe
        @SuppressWarnings("unused")
        private volatile int lock;
        public boolean f_open;
        private long f_a;
        private int f_b;
        private byte f_depth;
        private int f_rank;
        public boolean s_open;
        private long s_a;
        private int s_b;
        private byte s_depth;
        private int s_rank;

        public Block() {
            lock = 0;
            f_open = false;
            s_open = false;
        }

        private void lock() {
            // - maybe, if we ever get serious collisions (not likely), we can
            // make this abort instead of spinning.
            if (!unsafe.compareAndSwapInt(this, valueOffset, 0, 1)) {
                collisions++;
            }
        }

        private void unlock() {
            lock = 0;
        }

        public Long get(Ident in, byte depth) {
            lock();
            try {
                if (!f_open) {
                    failed++;
                    return null;
                }

                if (f_a == in.a && f_b == in.b && f_depth == depth) {
                    hit_first++;
                    return (long) f_rank;
                }

                if (!s_open) {
                    failed++;
                    return null;
                }

                if (s_a == in.a && s_b == in.b && s_depth == depth) {
                    hit_second++;
                    return (long) s_rank;
                }

                failed++;
                return null;

            } finally {
                unlock();
            }
        }

        public void add(Ident in, byte depth, int rank) {
            lock();
            try {
                if (!f_open) {
                    f_a = in.a;
                    f_b = in.b;
                    f_depth = depth;
                    f_rank = rank;
                    f_open = true;
                    return;
                }

                if (depth >= f_depth) {
                    s_open = true;
                    s_a = f_a;
                    s_b = f_b;
                    s_depth = f_depth;
                    s_rank = f_rank;

                    f_a = in.a;
                    f_b = in.b;
                    f_depth = depth;
                    f_rank = rank;
                } else {
                    s_open = true;
                    s_a = in.a;
                    s_b = in.b;
                    s_depth = depth;
                    s_rank = rank;
                }
            } finally {
                unlock();
            }
        }

        public void update(Ident in, byte depth, int rank) {
            lock();
            try {
                if (!f_open) {
                    f_a = in.a;
                    f_b = in.b;
                    f_depth = depth;
                    f_rank = rank;
                    f_open = true;
                    return;
                }

                // we replace if the new entry is better
                if (depth >= f_depth && f_a == in.a && f_b == in.b) {
                    f_rank = rank;
                    f_depth = depth;
                    return;
                }

                if (!s_open) {
                    s_open = true;
                    s_a = in.a;
                    s_b = in.b;
                    s_depth = depth;
                    s_rank = rank;
                    return;
                }

                if (depth >= s_depth && s_a == in.a && s_b == in.b) {
                    s_depth = depth;
                    s_rank = rank;
                }
                return;
            } finally {
                unlock();
            }
        }
    }
}
