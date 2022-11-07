package testJar;

import testJar.Addition;

import java.lang.reflect.*;


public class ReflectionUseCase {

    public void call_setAccessible1() throws ClassNotFoundException, NoSuchFieldException {
        Class<?> clazz = Class.forName("testJar.Addition");
        Field field = clazz.getDeclaredField("firstNumber");
        field.setAccessible(true);
    }

    public void call_setAccessible2() throws NoSuchFieldException {
        Class<?> clazz = Addition.class;
        clazz.getDeclaredField("secondNumber").setAccessible(true);
    }

    public static void call_set() throws NoSuchFieldException, IllegalAccessException {
        Class<?> clazz = Addition.class;
        Field secondNumber = clazz.getDeclaredField("secondNumber");
        secondNumber.setAccessible(true);
        Addition to_add = new Addition(5,3);
        secondNumber.set(to_add, 5);
        System.out.println(to_add.getResult());
    }

    public static void main(String[] args) throws NoSuchFieldException, IllegalAccessException {
        call_set();
    }


}
