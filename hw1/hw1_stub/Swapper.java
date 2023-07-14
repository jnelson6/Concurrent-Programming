
/* 
Author:      Julia Nelson
Date:        9/19/2021
File:        swapper.java
Description: Declares the Swapper Class
Pledge:      " I pledge my honor that I have abided by the Stevens Honor System."
*/

/* This class, which should implement the Runnable interface, will write to the 
    buffer given its Interval. It has the fields o↵set, interval, content, buffer: 

Offset: specifies the starting index in the buffer where the content will be placed. 
Interval: specifies the starting and ending index of the content in the original file 
            that is being swapped. Content: the entire original file in a String. 
Buffer: The shared char[] that the result is being written to.

*/
public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!

        // Write the specified content into the buffer. Helper methods may be used
        // to retrieve the content and insert it into the proper spot in the bu↵er.

        int size = interval.getY() - interval.getX();

        for( int i = 0; i < size + 1; i++ ) {

            buffer[i + offset ] = content.charAt( interval.getX() + i );
        }
        
    }
}





