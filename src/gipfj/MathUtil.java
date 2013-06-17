package gipfj;

public class MathUtil {
    public static long hexNum(long rad) {
        return 1 + 3 * rad * (rad - 1);
    }

    public static long reverseHexFloor(long in) {
        double d = (in - 1) / 3;
        double v = Math.sqrt(d);
        long iv = (int) v;

        if (hexNum(iv + 1) > in) {
            return iv;
        } else {
            return iv + 1;
        }
    }

    public static long[] getHexFloorArray(int size) {
        long[] res = new long[size];
        for (int i = 0; i < size; i++) {
            res[i] = reverseHexFloor(i);
        }
        return res;
    }
}
