//tws.groovy skeleton 
// 3 Way Stop 
// NEVER do a WAIT inside and IF STATEMENT ************* want it inside a WHILE Loop

class TWS {
	private int state;

	TWS() {
		state=1;

	}
	synchronized void printState(int i){

				println Thread.currentThread().getId()+ " calls "+i;
	}

	synchronized void one() {
		while(state!=1){`				/// NEVER DO A WAIT INSIDE AN IF STATEMENT *************
			wait()
		}
		state=2
		notifyAll()
		printState(1)

	}

	synchronized void two() {
		while(state!=2){
			wait()
		}
		state=3
		notifyAll()
		printState(2)

	}


	synchronized void three() {
		while(state!=3){
			wait()
		}
		state=1
		notifyAll()
		printState(3)

	}
}
TWS tws = new TWS();

200.times {
	Thread.start { //

		switch (new Random().nextInt(3)) of 
			case 0: 
				println Thread.currentThread().getId()+ " calls one";
				tws.one(); 
				break
			case 1: 
				println Thread.currentThread().getId()+ " calls two";
				tws.two(); 
				break
			defau;t: 
				println Thread.currentThread().getId()+ " calls three";
				tws.three(); 
	}
}




/********************* or separated version 

20.times {
	Thread.start { //
		tws.two()
	}
}
20.times {
	Thread.start { //
		tws.three()
	}
}
20.times {
	Thread.start { //
		tws.one()
	}
}

**********************/