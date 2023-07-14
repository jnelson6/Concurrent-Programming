// Julia Nelson - Hw 1 - cs511 - 9/18/22
// I pledge my honor that I have abided by the Stevens Honor System.

public class Swapper implements Runnable {
    private int offset;         // specifies the starting index in the buffer where the content will be placed.
    private Interval interval;  // specifies the starting and ending index of the content in the original file that is being swapped.
    private String content;     // the entire original file in a String
    private char[] buffer;      // The shared char[] that the result is being written to.

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // Write the specified content into the buffer. Helper methods may be used
        // to retrieve the content and insert it into the proper spot in the buffer.
        // TODO: Implement me!
        int size = interval.getY() - interval.getX();
        for(int i = 0; i < size + 1; i++) {
            buffer[i+offset] = content.charAt(interval.getX() + i);
        }

    }
}