import java.util.HashMap;

public class Bas {

    public static Object f0 = new Fun() {
        public Object apply(Object o) {
            HashMap m = (HashMap) o;
            return - (Integer) m.get("1");
        }
    };

    public static Object f1 = new Fun() {
        public Object apply(Object o) {
            HashMap m = (HashMap) o;
            return (Integer) m.get("1") + (Integer) m.get("2");
        }
    };

    public static Object f2 = new Fun() {
        public Object apply(Object o) {
            HashMap m = (HashMap) o;
            return (Integer) m.get("1") - (Integer) m.get("2");
        }
    };

    public static Object f3 = new Fun() {
        public Object apply(Object o) {
            HashMap m = (HashMap) o;
            return (Integer) m.get("1") * (Integer) m.get("2");
        }
    };

    public static Object f4 = new Fun() {
        public Object apply(Object o) {
            HashMap m = (HashMap) o;
            return (Integer) m.get("1") / (Integer) m.get("2");
        }
    };
}
