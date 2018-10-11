import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {
      def reduce(number: Double): Unit = {amount -= number;}
      def add(number: Double): Unit = {amount += number;}
      def getAmount: Double = {return amount;}
    }

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = {
      if (amount < 0) throw new IllegalArgumentException("Amount to withdraw must be positive");
      if (amount > balance.getAmount) throw new IllegalArgumentException("Cannot overdraw account. Balance must be greater than amount")
      balance.reduce(amount);
    }
    
    def deposit(amount: Double): Unit = {
      if (amount < 0) throw new IllegalArgumentException("Amount to deposit must be positive");
      balance.add(amount);
    }
    
    def getBalanceAmount: Double = {
      return balance.getAmount;
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
