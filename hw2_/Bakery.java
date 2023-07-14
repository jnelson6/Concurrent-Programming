/**
*
* Julia Nelson 
* Homework 2
* CS-511
* October 2, 2022
* "I pledge my honor that I have abided by the Stevens Honor System."
*
*

**/
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int CAPACITY = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    // TODO
    
    
    Semaphore perm_bakery = new Semaphore(CAPACITY);
    Semaphore perm_cashiers = new Semaphore(4);
    
    Semaphore mutex = new Semaphore(1);         

    Semaphore perm_RYE_shelf = new Semaphore(1);
    Semaphore perm_SOURDOUGH_shelf = new Semaphore(1);
    Semaphore perm_WONDER_shelf = new Semaphore(1);





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
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // TODO

        executor = Executors.newFixedThreadPool(CAPACITY);     // OR TOTAL_CUSTOMERS
        for (int i = 0; i < TOTAL_CUSTOMERS; i++){      // OR TOTAL_CUSTOMERS   // You may assume that there are a total of CAPACITY that are allowed in the bakery.
            executor.execute(new Customer(this));  
            
            }
            executor.shutdown();

            
            try{ 
                executor.awaitTermination(TOTAL_CUSTOMERS, TimeUnit.HOURS);   // OR CAPACITY 
                System.out.printf("Total sales = %.2f\n", sales); 
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            } 
        }
    }
            /*try {

                perm_bakery.acquire();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }  
            perm_bakery.release();


        }
        System.out.printf("Total sales = %.2f\n", sales);
        executor.shutdown();
        }
        */

    
/*
try{ 
                executor.awaitTermination(1, TimeUnit.HOURS);   // OR TOTAL_CUSTOMERS 
                System.out.printf("Total sales = %.2f\n", sales); 
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            } 
*/



        /**********************************




        // TODO
        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);
        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            executor.execute(new Customer(this));
        }
        executor.shutdown();

        // wait for all threads to finish
        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            try {
                customerPerm.acquire();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Total Sales: " + sales);





        ************************************

        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);
        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            
            executor.execute(new Customer(this)); 
        }
        executor.shutdown();
        try {
            executor.awaitTermination(TOTAL_CUSTOMERS, TimeUnit.SECONDS);
            System.out.println("Bakery sales: " + sales);
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }
    }
}

        *************************************/
























