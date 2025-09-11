CC=cc
CFLAGS=-std=c11 -Wall -Wextra -O0 -g

OBJS=main.o repl.o reader.o value.o env.o eval.o builtins.o syntax.o macros.o

all: minilisp

minilisp: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJS) minilisp

clean-obj:
	rm -f $(OBJS)
