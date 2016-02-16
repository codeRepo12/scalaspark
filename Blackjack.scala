object Blackjack {
   //Function to pick a random card
  def turn(s :member,x:Int)
  {
    for(i<-1 to x)
    {
    var pick=Deck.randomPick()
    println(s.getClass.getSimpleName+"'s card:"+pick("Symbol")+"-"+pick("Card"))
    s.addtoCount(Deck.sequence(pick("Card")))
    }
  }
   //Function for a new round of Black jack
  def newRound(gamblers :scala.collection.mutable.ArrayBuffer[Player],dealer :Dealer) =
  {
    println("Here we go ! Good luck !")
    var playerChoice=true
    // Initial cards for the players and the Dealer
    for(h<-0 to gamblers.size-1;if(gamblers(h).active==true))
    {
      println("Player "+h+" | Your cards are:")    
      turn(gamblers(h),2)
    }
    println("Dealers cards are :")
    turn(dealer,1)    
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
          turn(gamblers(u),1)
          }
        else playerChoice=false;
      }
    }
    // Dealer's turn 
    println("Dealers next cards are")
    while(dealer.Count<17) turn(dealer,1)
    
    for(p<-0 to gamblers.size-1;if(gamblers(p).active==true)) profits(gamblers(p))
    
    // Function that determines the net profit obtained by User for this round
    def profits(playerProfit :Player){
      if(dealer.Count>21)
      {
        if(playerProfit.Count<=21) playerProfit.remaining+=playerProfit.roundBet
      }      
      else
      {
         if(playerProfit.Count>21) playerProfit.remaining-=playerProfit.roundBet
         else
         {
           if(dealer.Count>playerProfit.Count) playerProfit.remaining-=playerProfit.roundBet
           else if(dealer.Count<playerProfit.Count) playerProfit.remaining+=playerProfit.roundBet 
           else playerProfit.remaining+=0
         }
      }
    }
    // Round conclusion 
    println("-------Remaining amount after this round--------- ")
    for(p<-0 to gamblers.size-1;if(gamblers(p).active==true)) 
      {
      println("Player "+p+": "+gamblers(p).remaining)
      gamblers(p).refresh()         
      }
    dealer.refresh()
  }
  
  
  
  def main(args:Array[String])
  {
     println("Welcome to the GreenFinch BlackJack Deck!")
     var players = scala.collection.mutable.ArrayBuffer.empty[Player]
     var deal=new Dealer()
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
             players(i).roundBet=betforNewRound
           else
            println("You dont have "+ betforNewRound +" in your account ")
         }
         else
         {
           players(i).active=false
           println("Player"+i+": U r leaving our deck with "+players(i).remaining + " dollars")
         }
       }
       if(gameOn==true) newRound(players,deal)       
     }
      
  }  
   
}
// Updated Deck and classes 
object Deck{
  val sequence=Map('2'->2,"3"->3,"4"->4,"5"->5,"6"->6,"7"->7,"8"->8,"9"->9,"10"->10,"J"->10,"Q"->10,"K"->10,"A"->11)
  val set=Map("Diamond"->sequence,"Spade"->sequence,"Club"->sequence,"Heart"->sequence)
  def randomPick()=
  {
    val random = scala.util.Random
    Map("Symbol"->set.keys.toVector(random.nextInt(set.size)),
        "Card"->sequence.keys.toVector(random.nextInt(sequence.size)))
  }
}
class Player(var totalBet:Int) extends member
{
   var active=true
   var roundBet=0
   var Count=0
   var remaining=totalBet
   def refresh()
   {
     if(this.remaining<=0) this.active==false
     this.Count=0
     this.roundBet=0   
   }
   override def addtoCount(d:Int)=this.Count+=d
 
}
class Dealer() extends member
{
  var Count=0
  override def addtoCount(d:Int)=this.Count+=d
  def refresh()=this.Count=0 
}

class member
{
  def addtoCount(k:Int)={}
}
