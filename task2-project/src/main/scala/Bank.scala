import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        BankManager.createAccount(accountId = accountCounter.incrementAndGet().toString, bankId = bankId, initialBalance = initialBalance)
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        try {
            Some(BankManager.findAccount(bankId = bankId, accountId = accountId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        try {
            Some(BankManager.findBank(bankId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
        case GetAccountRequest(id) => sender ! findAccount(accountId = id) // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
            val isInternal = t.toAccountNumber.length <= 4
            val toBankId = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
            val toAccountId = if (isInternal) t.toAccountNumber else t.toAccountNumber.substring(4)

            // Assume our recipient exists
            if (isInternal || toBankId == bankId) {
                findAccount(toAccountId).get ! t
            } else {
                findOtherBank(toBankId).get ! t
            }
        }

        case msg => ???
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status

        // TODO: Fix code reuse?
        if (isInternal || toBankId == bankId) {
            findAccount(toAccountId) match {
                case None =>
                    t.status = TransactionStatus.FAILED
                    sender ! TransactionRequestReceipt(t.from, t.id, t)
                case Some(account: ActorRef) =>
                    account ! t
            }
        } else {
            findOtherBank(toBankId) match {
                case None =>
                    t.status = TransactionStatus.FAILED
                    sender ! TransactionRequestReceipt(t.from, t.id, t)
                case Some(bank: ActorRef) =>
                    bank ! t
            }
        }
    }
}