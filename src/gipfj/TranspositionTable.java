package gipfj;

import java.util.HashMap;

public class TranspositionTable {
    private TranspositionTable() {
    }

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