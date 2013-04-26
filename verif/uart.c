void uart_putchar (int sym)
{
    char ch = sym;
    write (1, &ch, 1);
}
