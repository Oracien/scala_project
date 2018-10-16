import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {
      def reduce(number: Double): Unit = {amount -= number;}
      def add(number: Double): Unit = {amount += number;}
      def getAmount: Double = {return amount;}
    }

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = this.synchronized {
      if (amount < 0) throw new IllegalAmountException("Amount to withdraw must be positive");
      if (amount > balance.getAmount) throw new NoSufficientFundsException("Cannot overdraw account. Balance must be greater than amount")
      balance.reduce(amount)
    }
    
    def deposit(amount: Double): Unit = this.synchronized {
      if (amount < 0) throw new IllegalAmountException("Amount to deposit must be positive");
      balance.add(amount)
    }
    
    def getBalanceAmount: Double = {
      return balance.getAmount
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }

    override def toString:String =
        this.bank.uid + ("0"*4 + this.uid.toString takeRight 4)
}
