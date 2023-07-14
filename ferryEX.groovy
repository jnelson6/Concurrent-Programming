/****
**	EXERCISE BOOKLET 5 -- EXERCISE 3
** 	
**	West coast __________FERRY>_____________East Coast 
**	Must let everyone on the leave to other side 
**	Let  everyone off 
** 	Then let everyone on 
*****/



import java.util.concurrent.Semaphore;
 // Declare all necessary semaphores here
final int N = 20;			// ferry capacity 
permToBoard = [new Semaphore(0), new Semaphore(0)];	// list with 2 semaphores

 // any others 
permToContinue = new Semaphore(0); 
permToDisembark = new Semaphore(0);

Thread.start { // Ferry
	int coast =0;
 	while ( true ) {

 	// allow passengers on
 	N.times { permToBoard[coast].release(); };
 	N.times { permToContinue.aquire(); };
 	N.times { };
	// move to opposite coast
 	coast = 1- coast ;
	// wait for all passengers to get off
	N.times { permToDisembark.release(); };
	N.times { permToContinue.aquire();};
	}
 }

100.times {
	int my_coast = (new Random()).nextInt(1);
	Thread.start { // Passenger on East coast

 	permToBoard[0].aquire();
 	permToContinue.release();
 	// get on
	// get off at opposite coast
	permToDisembark.aquire();
	permToContinue.release();
 	}
}



100.times {
	Thread.start { // Passenger on West coast

 	permToBoard[0].aquire();
 	permToContinue.release();
 	// get on
	// get off at opposite coast
	permToDisembark.aquire();
	permToContinue.release();
	}
}
return;












/********


class PC {
	Objuect buffer;

	synchronized void produce(Object o) {
		buffer = 0;
		}
		}

		synchronized void consume() {
		Object o = buffer;
		buffer = null;
		return o;
		}
		}
		***********/ 