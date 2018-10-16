import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {
        def reduce(number: Double): Unit = {amount -= number;}
        def add(number: Double): Unit = {amount += number;}
        def getAmount: Double = {amount;}
    }

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        transactions.values.toList
    }

    def allTransactionsCompleted: Boolean = {
        getTransactions.forall(t => t.isCompleted)
    }

    def withdraw(amount: Double): Unit = this.synchronized {
        if (amount < 0) throw new IllegalAmountException("Amount to withdraw must be positive")
        if (amount > balance.getAmount) throw new NoSufficientFundsException("Cannot overdraw account. Balance must be greater than amount")
        balance.reduce(amount)
    }

    def deposit(amount: Double): Unit = this.synchronized {
        if (amount < 0) throw new IllegalAmountException("Amount to deposit must be positive")
        balance.add(amount)
    }

    def getBalanceAmount: Double = {
        balance.getAmount
    }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Assume our own bank exists.
        val bank: ActorRef = BankManager.findBank(bankId)
        bank ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
        case IdentifyActor => sender ! this

        case TransactionRequestReceipt(to, transactionId, transaction) =>
            // Assume transactionId and transaction is valid
            transactions(transactionId).status = transaction.status
            transactions(transactionId).receiptReceived = true

        case BalanceRequest => sender ! getBalanceAmount

        case t: Transaction =>
            try {
                deposit(t.amount)
                t.status = TransactionStatus.SUCCESS
            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
            sender ! TransactionRequestReceipt(toAccountNumber = t.from, transactionId = t.id,
                transaction = t)


        case msg => ???
    }


}
