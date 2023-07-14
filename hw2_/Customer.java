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
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import java.util.ArrayList;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        this.bakery = bakery;
        rnd = new Random();
        shoppingCart = new ArrayList<BreadType>();
        this.shopTime = rnd.nextInt(1)+1;   //2000
        this.checkoutTime = rnd.nextInt(1)+1;   //1000
        fillShoppingCart();




    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        for (int i = 0; i < shoppingCart.size(); i++) {
            System.out.println(toString() + " has started shopping."); // System.out.println("Customer " + hashCode() + " has began shopping.");
            try {
                Thread.sleep(shopTime);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }


            if (shoppingCart.get(i) == BreadType.SOURDOUGH){
                try {
                    this.bakery.perm_SOURDOUGH_shelf.acquire();
                    bakery.takeBread(shoppingCart.get(i));  // this.bakery.takeBread(BreadType.SOURDOUGH);
                    this.bakery.perm_SOURDOUGH_shelf.release();
                    System.out.println("Customer " + hashCode() + " took 1 loaf of SOURDOUGH off the shelf!");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            } else if (shoppingCart.get(i) == BreadType.RYE) {
                try {
                    this.bakery.perm_RYE_shelf.acquire();
                    bakery.takeBread(shoppingCart.get(i));  // this.bakery.takeBread(BreadType.RYE);
                    this.bakery.perm_RYE_shelf.release();
                    System.out.println("Customer " + hashCode() + " took 1 loaf of RYE off the shelf!");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            } else if (shoppingCart.get(i) == BreadType.WONDER) {
                try {
                    this.bakery.perm_WONDER_shelf.acquire();
                    bakery.takeBread(shoppingCart.get(i));  // this.bakery.takeBread(BreadType.RYE);
                    this.bakery.perm_WONDER_shelf.release();
                    System.out.println("Customer " + hashCode() + " took 1 loaf of WONDER off the shelf!");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
        try{
            float value = this.getItemsValue();
            this.bakery.perm_cashiers.acquire();
            System.out.println("Customer " + hashCode() + " checking out, total cart value = " + value);
            this.bakery.mutex.acquire();       
            bakery.addSales(getItemsValue());
            this.bakery.mutex.release();
            
            Thread.sleep(checkoutTime);

        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        this.bakery.perm_cashiers.release();
        System.out.println("Customer " + hashCode() + " has finished and left the Bakery.");
    }


    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}