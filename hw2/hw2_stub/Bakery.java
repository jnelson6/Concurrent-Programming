import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int ALLOWED_CUSTOMERS = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    Semaphore sourdoughShelf = new Semaphore(1);
    Semaphore ryeShelf = new Semaphore(1);
    Semaphore wonderShelf = new Semaphore(1);
    Semaphore canEnterBakery = new Semaphore(ALLOWED_CUSTOMERS);
    Semaphore canUpdateSales = new Semaphore(1);
    Semaphore registers = new Semaphore(4);

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        //only 20 loaves of each type of bread
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // Get the executor with 50 threads in its thread pool
        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);
        for (int i = 0; i < TOTAL_CUSTOMERS; i++)
        {
            try 
            {
                canEnterBakery.acquire();
            } catch (InterruptedException e) 
            {
                e.printStackTrace();
            }
            executor.execute(new Customer(this));
            
            canEnterBakery.release();
        }
        
        System.out.println("TOTAL BAKERY SALES = " + sales);

        executor.shutdown();


        //executor.shutdown();
        //System.out.println("TOTAL BAKERY SALES = " + sales);
    }
}