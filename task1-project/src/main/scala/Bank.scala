import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = 2001 //TODO: fix
    private var counter = 0
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val pool: ForkJoinPool = new ForkJoinPool(4)
    private val executorContext = ExecutionContext.fromExecutorService(pool)
    
    

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
      executorContext.execute(new Runnable {
        def run() = {
            Main.thread(processTransactions)
        }
      })
    }

    // Hint: use a counter 
    def generateAccountId: Int = this.synchronized{
      counter = counter +1
      return counter
    }

    private def processTransactions: Unit = this.synchronized {
      while (!transactionsQueue.isEmpty) {
        //println("Queue is " + transactionsQueue.length + " long")
        val transaction: Transaction = transactionsQueue.pop
        if(transaction.status != TransactionStatus.SUCCESS) {
          transaction.run
          if (transaction.status == TransactionStatus.FAILED && transaction.allowedAttempts > 1) {
            //println("Trying again with " + transaction.allowedAttempts + " attempts left")
            val newTransaction: Transaction = new Transaction(transactionsQueue, processedTransactions, transaction.from, transaction.to, transaction.amount, transaction.allowedAttempts -1)
            newTransaction.status == TransactionStatus.FAILED
            transactionsQueue.push(newTransaction)
          } else {
          processedTransactions.push(transaction)
          }
        }
      }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
