// TRAINS

import java.util.concurrent.locks.*;

// declarations
class TrainStation {
    int ntc=0; // for printing state
    int stc=0; // for printing state

    boolean nt=false;
    boolean st=false;

    // declare locks and condition variables
	Lock lock = new ReentrantLock();
	Condition northTrack = lock.newCondition();
	Condition southTrack = lock.newCondition();
	Condition bothTracks = lock.newCondition();

    void acquireNorthTrackP() {
		lock.lock();
		try {
			// complete
			while(nt) {
				northTrack.await();
			}
			nt = true;
			ntc++;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void releaseNorthTrackP() {
		lock.lock();
		try {
			// complete
			nt = false;
			northTrack.signal();
			bothTracks.signal();
			ntc--;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void acquireSouthTrackP() {
		lock.lock();
		try {
			// complete
			while(st) {
				southTrack.await();
			}
			st = true;
			stc++;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void releaseSouthTrackP() {
		lock.lock();
		try {
			// complete
			st = false;
			southTrack.signal();
			bothTracks.signal();
			stc--;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void acquireTracksF() {
	lock.lock();
        try {
			// complete
			while(nt || st) {
				bothTracks.await();
			}
			nt = true;
			st = true;
			ntc++;
			stc++;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void releaseTracksF() {
	lock.lock();
        try {
			// complete
			nt = false;
			st = false;
			bothTracks.signal();
			northTrack.signal();
			southTrack.signal();
			ntc--;
			stc--;
			printState();
		} finally {
			lock.unlock();
		}
    }

    void printState() {
		lock.lock();
		try {
			println "ntc: "+ntc+", stc: "+stc;	    
		} finally {
			lock.unlock();
		}
    }
}

TrainStation s = new TrainStation();

10.times {
    int id = it;
    Thread.start { // Freight Train going in any direction
		s.acquireTracksF();
		s.releaseTracksF();
    }
}

100.times{
    int id = it;
    Thread.start { // Passenger Train going North
		s.acquireNorthTrackP();
		s.releaseNorthTrackP();
    }
}

100.times{
    int id = it;
    Thread.start { // Passenger Train going South
		s.acquireSouthTrackP();
		s.releaseSouthTrackP();
    }
}