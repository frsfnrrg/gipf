package gipfj;

// designed for clojure consumption; please use only longs
public class MathUtil {
    private MathUtil() {
    }

    public static Long add(Long a, Long b) {
        return a + b;
    }

    public static Long multiply(Long a, Long b) {
        return a * b;
    }

    public static Long divide(Long a, Long b) {
        return a / b;
    }

    public static Long subtract(Long a, Long b) {
        return a - b;
    }

    public static Long inc(Long a) {
        return a + 1;
    }

    public static Long dec(Long a) {
        return a - 1;
    }

    public static Long negate(long a) {
        return -a;
    }

    public static Boolean equals(long a, long b) {
        return a == b;
    }

    public static Long max(long a, long b) {
        return (a > b) ? a : b;
    }

    public static Long min(long a, Long b) {
        return (a < b) ? a : b;
    }

    public static Boolean greater(Long a, Long b) {
        return (a > b);
    }

    public static Boolean less(Long a, Long b) {
        return (a < b);
    }

    public static Boolean greaterEquals(Long a, Long b) {
        return (a >= b);
    }

    public static Boolean lesserEquals(Long a, Long b) {
        return (a <= b);
    }

    public static Boolean evenp(Long a) {
        return 0 == (a % 2);
    }

    public static Boolean oddp(Long a) {
        return 1 == (a % 2);
    }

    // primitive math section

    public static long ladd(long a, long b) {
        return a + b;
    }

    public static long lmultiply(long a, long b) {
        return a * b;
    }

    public static long ldivide(long a, long b) {
        return a / b;
    }

    public static long lsubtract(long a, long b) {
        return a - b;
    }

    public static long linc(long a) {
        return a + 1;
    }

    public static long ldec(long a) {
        return a - 1;
    }

    public static long lnegate(long a) {
        return -a;
    }

    public static boolean lequals(long a, long b) {
        return a == b;
    }

    public static long lmax(long a, long b) {
        return (a > b) ? a : b;
    }

    public static long lmin(long a, long b) {
        return (a < b) ? a : b;
    }

    public static boolean lgreaterEquals(long a, long b) {
        return (a >= b);
    }

    public static boolean llesserEquals(long a, long b) {
        return (a <= b);
    }

    public static boolean lgreater(long a, long b) {
        return (a > b);
    }

    public static boolean lless(long a, long b) {
        return (a < b);
    }

    public static boolean levenp(long a) {
        return 0 == (a % 2);
    }

    public static boolean loddp(long a) {
        return 1 == (a % 2);
    }
}
