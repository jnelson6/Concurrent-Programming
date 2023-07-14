
/* 
Author:      Julia Nelson
Date:        9/19/2021
File:        TextSwap.java
Description: Declares the TestSwap Class. this class holds the main method from which the assignment is executed.
Pledge:      " I pledge my honor that I have abided by the Stevens Honor System."
*/

import java.io.*;
import java.util.*;

public class TextSwap {



    /*
        This method should read from a specified file by placing it into a String- Builder 
        object and then returning the toString() of that object.
    */
    private static String readFile(String filename, int chunkSize) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
    // The "-1" below is because of this:
    // https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
    if ((file.length()-1) % chunkSize!=0)
        { throw new Exception("File size not multiple of chunk size"); };
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }






    /* 
        This method returns an array of “Intervals”. 
        An interval is just a pair of integers that specify the start and end index of a chunk.
        These intervals will be delegated to the appropriate threads to ensure the reordering 
        is proper.
    */
        /* NOTES from discussion board.....

        Thread myThread1 = new Thread(new MyRunnableClass( /~ Arguments Here ~/ ));
        Thread myThread2 = new Thread(new MyRunnableClass( /~ Arguments Here ~/ ));
        myThread1.start();  // starts thread and continues
        myThread2.start();  // starts thread and continues
        myThread1.join();   // Waits for thread to finish executing 
        myThread2.join();   // Waits for thread to finish executing 
    */
    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        // TODO: Implement me!
        Interval[] intervalsArray = new Interval[numChunks];

        int start = 0; 

        for (int i = 0; i < numChunks; i++) { 
            start = start + chunkSize; 
            // start = i; 
            Interval temp = new Interval(start, (start + chunkSize) - 1 );
            intervalsArray[i] = temp;
            // end = (i + chunkSize - 1);
            //start = start + chunkSize; // chunkSize rather than chunkSize-1 because it brings to next start index not end 
            // i = i + (chunkSize - 1);
        }
        return intervalsArray;
    }
















    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }







    /*
        This method does much of the actual logic of the class. 
        It creates the intervals, 
        runs the Swapper threads, and 
        returns the reordered buffer that will be written to the new file.
    */
    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);

        // TODO: 
        // Order the intervals properly, 
        
        // buffer
        char[] buffer = new char[chunkSize * numChunks];    // or content.length() ???
        // int i = 0 to num intervals ; i++
        // while intervals[i]
        Thread[] new_Order = new Thread[numChunks];    // intervals.length() // creates a sort of list/buffer to sort intervals into  ---- numChunks or intervals.??????
        // ____________________
        // take labels (entered by user) then run the Swapper instances.

        for (int i = 0; i < numChunks; i++){                 // 26 or numChunks???? or labels.length() or ____labels.size()_____ ????????/
                    // new_Order[i]
            new_Order[i] = new Thread(new Swapper(intervals[labels.get(i) - 'a'], content, buffer, chunkSize * i )); 
            new_Order[i].start();      // new_Order[i].start() should i have [i] if im doing thread[] 
            //new_Order.join(); 
        }
        for (int i = 0; i<numChunks; i++){ 
            // given in turnstile example
            try {
                new_Order[i].join();               // new_Order[i].join()
            } catch (InterruptedException e) {
                e.printStackTrace();
                  // do i need throw e;  
            }
        }
            return buffer;
    }








    /* 
        This method writes the buffer to a new file.
    */
    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }



    /*
        The main should parse two command line inputs, a chunk size and a filename. 
        
        The size of the file and the number of chunks should then be calculated, and 
        the new pattern of letters should be read from stdin. 
        
        - If the number of chunks is more than 26, then execution should halt with an 
        error message “Chunk size too small”. 
        
        - If the file size is not a multiple of the chunk size, then execution should 
        halt with an error message “File size must be a multiple of the chunk size”. 
        
        Note that there may be other methods necessary to complete this class, 
        but they can also be inlined into these methods.
    */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1],chunkSize);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            System.out.println("Error with IO.");
            return;
        }
    }
}