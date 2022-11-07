package testJar;

public class Addition {
    private int firstNumber = 0;
    private int secondNumber = 0;

    Addition(int a, int b){
        firstNumber = a;
        secondNumber = b;
    }

    private int compute(){
        return firstNumber + secondNumber;
    }

    public int getResult(){
        return compute();
    }

    public static int add(int a, int b){
        return a + b;
    }

}
