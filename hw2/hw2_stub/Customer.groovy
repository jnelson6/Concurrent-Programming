// made some fixes, I'm pretty sure these semaphores are in the right order, 
// I know our executors/executorservice isn't right and we need to handle 
// exceptions around acquiring semaphores!


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

    //available functions:
    //addItem, fillShoppingCart, getItemsValue, Bakery.addSales()
    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingCart = new ArrayList<BreadType>();
        this.shopTime = rnd.nextInt(5000);
        this.checkoutTime = rnd.nextInt(2000);
        fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        //begin shopping
        System.out.println(this.toString());
        float value = this.getItemsValue();
        //int price = 0; or bakery.addSale(value) 

        //pull items from stock
        for (int i = 0; i < shoppingCart.size(); i++) {

            if (this.shoppingCart.get(i) == BreadType.SOURDOUGH)
            {
                this.bakery.sourdoughShelf.acquire();
                this.bakery.takeBread(BreadType.SOURDOUGH);                                 // IS THIS THE CORRECT format of parameter
                System.out.println("printout message fill in for SOURDOUGH");
                Thread.sleep(shopTime);
                this.bakery.sourdoughShelf.release();

            }

            else if (this.shoppingCart.get(i) == BreadType.RYE)
            {
                this.bakery.ryeShelf.acquire();
                this.bakery.takeBread(BreadType.RYE);                                       // IS THIS THE CORRECT format of parameter
                System.out.println("printout message fill in for RYE ");
                Thread.sleep(shopTime);                    // DO WEE NEED TO ADD SOMETHING FOR PRICE?
                this.bakery.ryeShelf.release();
                
            }

            else if (this.shoppingCart.get(i) == BreadType.WONDER)
            {
                this.bakery.wonderShelf.acquire();
                this.bakery.takeBread(BreadType.WONDER);                                   // IS THIS THE CORRECT format of parameter
                System.out.println("printout message fill in for WONDER");
                Thread.sleep(shopTime);                     // DO WEE NEED TO ADD SOMETHING FOR PRICE?
                this.bakery.wonderShelf.release();
                
            }

        }
        
        // NEED TO THEN RUN CHECKOUT PROCESS
        this.bakery.registers.acquire();      
        System.out.println("fill in with proper message ");
        this.bakery.canUpdateSales.acquire();
        this.bakery.addSales(value);
        this.bakery.canUpdateSales.release();
        Thread.sleep(checkoutTime);
        this.bakery.registers.release();

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
     * Adding stuff as you go with list in hand at the grocery store
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