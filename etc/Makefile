SCAL_DEPS := scal.cpp shared_ptr.cpp

scal: $(SCAL_DEPS)
	g++ -o $@ $(SCAL_DEPS)

hello: hello.rs
	rustc $<

jit1: jit1.c
	gcc -o $@ $<

play: play.go
	go build $<

pthread1: pthread1.c
	$(CC) -o $@ $< -lpthread

timer1: timer1.c
	$(CC) -o $@ $< -lrt

clean:
	$(RM) scal hello jit1 play pthread1 timer1

.PHONY: clean
