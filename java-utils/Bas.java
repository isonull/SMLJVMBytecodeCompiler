import java.util.HashMap;
import java.lang.String;
import java.lang.Integer;
import java.lang.reflect.Array;

public class Bas {

    public static Object f96 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] + (Integer) m[1];
        }
    };

    public static Object f100 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] - (Integer) m[1];
        }
    };

    public static Object f104 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] * (Integer) m[1];
        }
    };

    public static Object f108 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] / (Integer) m[1];
        }
    };

    public static Object f112 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] % (Integer) m[1];
        }
    };
    
    public static Object f116 = new Fun() {
        public Object apply(Object o) {
            return - (Integer) o;
        }
    };

    public static Object f159 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] == (Integer) m[1];
        }
    };

    public static Object f160 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] != (Integer) m[1];
        }
    };

    public static Object f161 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] < (Integer) m[1];
        }
    };

    public static Object f162 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] >= (Integer) m[1];
        }
    };

    public static Object f163 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] > (Integer) m[1];
        }
    };

    public static Object f164 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return (Integer) m[0] <= (Integer) m[1];
        }
    };

    public static Object f0 = new Tag(0, null);

    public static Object f1 = new Tag(1, null);

    public static Object f2 = new Fun() {
        public Object apply(Object o) {
            return (new Tag(0, o));
        }
    };

    public static Object f3 = new Fun() {
        public Object apply(Object o) {
            return (new Object[]{o});
        }
    };

    public static Object f4 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return m[0];
        }
    };

    public static Object f5 = new Fun() {
        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            Object[] r = (Object[]) m[0];
            r[0] = m[1];
            return new Object[0];
        }
    };

    public static Object f6 = new Fun() {

        public boolean arreq(Object[] o1, Object[] o2) {
            for (int i = 0; i < o1.length; i++) {
                if (eq (o1[i], o2[i])) return false;
            }
            return true;
        }

        public boolean tageq(Tag o1, Tag o2) {
            return (o1.tag == o2.tag) && eq(o1.val, o2.val);
        }

        public boolean eq(Object o1, Object o2) {
            if (o1 instanceof Integer)
                return ((Integer) o1).equals((Integer) o2);
            else if (o1 instanceof String)
                return ((String) o1).equals((String) o2);
            else if (o1 instanceof Tag)
                return tageq((Tag) o1, (Tag) o2);
            else if (o1 instanceof Array)
                return arreq((Object[]) o1, (Object[]) o2);
            return false;
        }

        public Object apply(Object o) {
            Object[] m = (Object[]) o;
            return eq(m[0], m[1]) ? f1 : f0;
        }
    };

    public static Object f7 = new Fun() {
        public Object apply(Object o) {
            System.out.println(o);
            return new Object[0];
        }
    };

    public static Object f8 = new Fun() {
        public Object apply(Object o) {
            return o.toString();
        }
    };

}
