package gipfj;

// designed for clojure consumption; please use only longs
public class MathUtil {
    private MathUtil() {

    }

    public static long add(long a, long b) {
        return a + b;
    }

    public static long multiply(long a, long b) {
        return a * b;
    }

    public static long divide(long a, long b) {
        return a / b;
    }

    public static long subtract(long a, long b) {
        return a - b;
    }

    public static long inc(long a) {
        return a + 1;
    }

    public static long dec(long a) {
        return a - 1;
    }

    public static long negate(long a) {
        return -a;
    }

    public static boolean equals(long a, long b) {
        return a == b;
    }

    public static long max(long a, long b) {
        return (a > b) ? a : b;
    }

    public static long min(long a, long b) {
        return (a < b) ? a : b;
    }

    public static boolean greater(long a, long b) {
        return (a > b);
    }

    public static boolean less(long a, long b) {
        return (a > b);
    }

    public static boolean evenp(long a) {
        return 0 == (a % 2);
    }

    public static boolean oddp(long a) {
        return 1 == (a % 2);
    }
}
