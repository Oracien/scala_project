import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
  
    var transactions = mutable.Queue[Transaction]();

    // Remove and return the first element from the queue
    def pop: Transaction = this.synchronized {
      return transactions.dequeue
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {return transactions.isEmpty}

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = this.synchronized {transactions.enqueue(t)}

    // Return the first element from the queue without removing it
    def peek: Transaction = {
      val copTransactions = transactions.clone
      return copTransactions.dequeue
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
      return transactions.iterator
    }
    
    def length: Int = {
      return transactions.length
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

      def doTransaction() = {
        try {
          from withdraw amount
          to deposit amount
          status = TransactionStatus.SUCCESS
        } catch {
          case e: NoSufficientFundsException => status = TransactionStatus.FAILED;
          case e: IllegalAmountException => status = TransactionStatus.FAILED;
        }
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }

      // Extend this method to satisfy requirements.

    }
  
  override def toString: String = {
    return "Transfering " + amount + " from " + from + " to " + to + " with " + allowedAttempts + " remaining"
  }
}
