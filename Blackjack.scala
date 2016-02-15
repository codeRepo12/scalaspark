object Blackjack {
  //Function for a new round of Black jack
  def newRound(gamblers :scala.collection.mutable.ArrayBuffer[Player]) =
  {
    println("Here we go ! Good luck !")
    var dealerCount=0
    var playerChoice=true
    // Initial cards for the players and the Dealer
    for(h<-0 to gamblers.size-1;if(gamblers(h).active==true))
    {
      println("Player "+h+" | Your cards are:")    
      playerTurn(gamblers(h))
      playerTurn(gamblers(h))
    }
    println("Dealers cards are :")
    dealerTurn()
    println('X')
    // Upon a nod from the user to continue a new card is retrieved !
    for(u<-0 to gamblers.size-1;if(gamblers(u).active==true))
    {
      playerChoice=true
      while(playerChoice==true)
      {
        println("Player "+u+" | Would you like to continue ? Press Y/N" )
        var choice=readLine()
        if(choice=="Y") {
          println("New card is:")
          playerTurn(gamblers(u))
          }
        else playerChoice=false;
      }
      gamblers(u).playerCount=gamblers(u).playerCounts.last
    }
    // Dealer's turn 
    println("Dealers next cards are")
    while(dealerCount<17) dealerTurn()
    
    for(p<-0 to gamblers.size-1;if(gamblers(p).active==true)) profits(gamblers(p))
    
    // Function that determines the net profit obtained by User for this round
    def profits(playerProfit :Player){
      if(dealerCount>21)
      {
        if(playerProfit.playerCount<=21) playerProfit.remaining+=playerProfit.roundBet
      }
      else
      {
         if(playerProfit.playerCount>21) playerProfit.remaining-=playerProfit.roundBet
         else
         {
           if(dealerCount>playerProfit.playerCount) playerProfit.remaining-=playerProfit.roundBet
           else if(dealerCount<playerProfit.playerCount) playerProfit.remaining+=playerProfit.roundBet 
           else playerProfit.remaining+=0
         }
      }
    }
    // Player and Dealer functions for retrieving a new card 
    
    def playerTurn(member :Player){
      var value=Deck.randomPick()
      value match
      {
        case x if(x>=0 && x<11) =>member.playerCounts=member.playerCounts map {_+x};println(x)
        case 74 => member.playerCounts=member.playerCounts map {_+10};println('J')
        case 81 => member.playerCounts=member.playerCounts map {_+10};println('Q')
        case 75 =>  member.playerCounts=member.playerCounts map {_+10};println('K')
        case 65 => member.playerCounts=member.playerCounts map {_+1};
                   member.playerCounts=member.playerCounts++(member.playerCounts map {_+10});
                   for(h<-member.playerCounts;if(h>21)) {member.playerCounts-=h};
                   println('A')
      }
    }
    
    def dealerTurn(){
      var value=Deck.randomPick()
      value match
      {
        case x if(x>=0 && x<11) => dealerCount+=x;println(x)
        case 74 => dealerCount+=10;println('J')
        case 81 => dealerCount+=10;println('Q')
        case 75 => dealerCount+=10;println('K')
        case 65 => dealerCount+=11;println('A')
      }
    }
    println("Remaining amount after this round ")

    for(p<-0 to gamblers.size-1;if(gamblers(p).active==true)) 
      {
      println("Player "+p+": "+gamblers(p).remaining)
      gamblers(p).refresh()         
      }
    
  }
  
  
  
  def main(args:Array[String])
  {
     println("Welcome to the GreenFinch BlackJack Deck!")
     var players = scala.collection.mutable.ArrayBuffer.empty[Player]
     println("How many are you game today ?")
     var num=readLine().toInt
     // Players total Amount
     for(i<-0 to num-1) 
       {
       println("You are Player-"+i+" | Enter the total amount you wanna play today !")
       var bet=readLine().toInt
       players+=new Player(bet)
       }
     // Function to check if any of the players have amount left to play
     def gameOn() : Boolean=
     {
       var res=false
       for(i<-players) res=res||i.active
       res
     }
     //While any player is active the game goes on 
     while(gameOn==true)
     {
       for(i<-0 to players.size-1; if(players(i).active==true))
       {
         println("Player:"+i+" |Lets start a new Round ! Press Y/N | You now have "+players(i).remaining)
         var playnewRound=readLine()
         if(playnewRound=="Y")
         {
           println("Place a bet to start a new Round !")
           var betforNewRound=readLine().toInt
           if(betforNewRound<=players(i).remaining)
           {
             players(i).roundBet=betforNewRound
             //f.remaining+=newRound(betforNewRound);
             //if(f.remaining<0) game=false
           }
           else
            println("You dont have "+ betforNewRound +" in your account ")
         }
         else
         {
           players(i).active=false
           println("Player"+i+": U r leaving our deck with "+players(i).remaining + " dollars")
         }
       }
       newRound(players);       
         
     }
      
  }  
   
}

object Deck{
  val sequence=List(2,3,4,5,6,7,8,9,10,'J','Q','K','A')
  val set= for(i<-sequence;k<-1 to 4) yield(i)
  def randomPick()=
  {
    val random = scala.util.Random
    sequence(random.nextInt(sequence.size))
  }
}
class Player(var totalBet:Int)
{
   var active=true
   var roundBet=0
   var playerCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
   playerCounts+=0
   var playerCount=0
   var remaining=totalBet
   def refresh()
   {
     if(this.remaining<=0) this.active==false
     this.playerCounts.reduceToSize(0)
     this.playerCounts+=0
     this.playerCount=0
     this.roundBet=0   
   }
}