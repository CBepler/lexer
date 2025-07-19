/*
 * This is a multi-line comment.
 * It can span across several lines.
 */
int main() { // This is a single-line comment
    int x = 10;
    int y = 20;

    /* Another
       multi-line comment */
    if (x == y) {
        return 0;
    } else {
        while (x < y) {
            x = x + 1; // Increment x
        }
        return x;
    }
}