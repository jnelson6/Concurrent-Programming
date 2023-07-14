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
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingCart = new ArrayList<BreadType>();
        this.shopTime = rnd.nextInt(2000);
        this.checkoutTime = rnd.nextInt(1000);
        fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        //begin shopping
        //System.out.println(this.toString());
        float value = this.getItemsValue();
        //pull items from stock
        for (int i = 0; i < shoppingCart.size(); i++) 
        {

            System.out.println(toString() + " has started shopping.");
            if (this.shoppingCart.get(i) == BreadType.SOURDOUGH)
            {
                try 
                {
                    this.bakery.sourdoughShelf.acquire();
                    this.bakery.takeBread(BreadType.SOURDOUGH);
                    System.out.println("Customer " + hashCode() + " took 1 loaf of SOURDOUGH off the shelf!");
                    Thread.sleep(shopTime);
                    this.bakery.sourdoughShelf.release();
                } 
                catch (InterruptedException e) 
                {
                    e.printStackTrace();
                }
            }

            else if (this.shoppingCart.get(i) == BreadType.RYE)
            {
                try 
                {
                    this.bakery.ryeShelf.acquire();
                    this.bakery.takeBread(BreadType.RYE);
                    System.out.println("Customer " + hashCode() + " took 1 loaf of RYE off the shelf!");
                    Thread.sleep(shopTime);
                    this.bakery.ryeShelf.release();
                } 
                catch (InterruptedException e) 
                {
                    e.printStackTrace();
                }
            }

            else if (this.shoppingCart.get(i) == BreadType.WONDER)
            {
                try 
                {
                    this.bakery.wonderShelf.acquire();
                    this.bakery.takeBread(BreadType.WONDER);                                 
                    System.out.println("Customer " + hashCode() + " took 1 loaf of WONDER off the shelf!");
                    Thread.sleep(shopTime);
                    this.bakery.wonderShelf.release();
                } catch (InterruptedException e) 
                {
                    e.printStackTrace();
                }
            }
        }
        
        // NEED TO THEN RUN CHECKOUT PROCESS
        try 
        {
            this.bakery.registers.acquire();
        } catch (InterruptedException e) 
        {
            e.printStackTrace();
        }      
        System.out.println("Customer " + hashCode() + " checking out, total cart value = " + value);
        
        try 
        {
            this.bakery.canUpdateSales.acquire();
            this.bakery.addSales(getItemsValue());
            this.bakery.canUpdateSales.release();
        } catch (InterruptedException e) 
        {
            e.printStackTrace();
        }
        try
        {
            Thread.sleep(checkoutTime);
        } catch (InterruptedException e) 
        {
            e.printStackTrace();
        }      
        this.bakery.registers.release();
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