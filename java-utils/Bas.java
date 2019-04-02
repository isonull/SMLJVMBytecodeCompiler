import java.util.HashMap;

public class Bas {

    public static Object f0 = new Fun() {
        public Object apply(Object o) {
            return - (Integer) o;
        }
    };

    public static Object f1 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] + (Integer) m[1];
        }
    };

    public static Object f2 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] - (Integer) m[1];
        }
    };

    public static Object f3 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] * (Integer) m[1];
        }
    };

    public static Object f4 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] / (Integer) m[1];
        }
    };
    
    public static Object f5 = new Fun() {
        public Object apply(Object o) {
            System.out.println(o);
            return (new Object[0]);
        }
    };

    public static Object f6 = new Tag(0, null);

    public static Object f7 = new Tag(1, null);

    public static Object f8 = new Tag(0, null);

    public static Object f9 = new Tag(1, null);
}
