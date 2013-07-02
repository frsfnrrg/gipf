package gipfj;

// java internal, int, math only
class IMath {
    public static int hexNum(int rad) {
        return 1 + 3 * rad * (rad - 1);
    }

    public static int reverseHexFloor(int in) {
        double d = (in - 1) / 3;
        double v = Math.sqrt(d);
        int iv = (int) v;

        if (hexNum(iv + 1) > in) {
            return iv;
        } else {
            return iv + 1;
        }
    }

    public static int[] getHexFloorArray(int size) {
        int[] res = new int[size];
        for (int i = 0; i < size; i++) {
            res[i] = reverseHexFloor(i);
        }
        return res;
    }

    public static int abs(int q) {
        if (q > 0) {
            return q;
        } else {
            return -q;
        }
    }
}
