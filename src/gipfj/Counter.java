package gipfj;

/**
 * 
 * Class purely for optimization - the (swap! atoooom inc) is horribly slow.
 * Here, we really are just mutating - so let it be Coffee.
 * 
 * Note: synchronization really isn't worth it, especially in a bottleneck like
 * this.
 */
public class Counter {
    private int i;

    public Counter() {
        i = 0;
    }

    public void inc() {
        i++;
    }

    public int get() {
        return i;
    }

    public void clear() {
        i = 0;
    }

    public static void cinc(Counter c) {
        c.inc();
    }

    public static long cget(Counter c) {
        return c.get();
    }

    public static void cclear(Counter c) {
        c.clear();
    }

    public static Counter cmake() {
        return new Counter();
    }
}
