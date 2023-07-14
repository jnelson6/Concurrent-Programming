Object[] buffer; // buff of size n
final int P = 100;
final int Q = 20;

Semaphore consume = new Semaphore(0);
Semaphore produce = new Semaphore(N);
Semaphore ,utexP = new Semaphore(??);

Thread.start { // producer
	while(true) { 
		produce.aquire();
		mutec=>
		buffer[start]=produceItem(?????????
			);
		start = (start + 1) % N;
		consume.release();

	}
}

Thread.start { // consumer
	while(true) { 
		consume.aquire();
		consumeItem(buffer[rear]);
		rear = (rear + 1) % N;
		produce.release();
	}
}
r.time { 
Thread.start { r....}}