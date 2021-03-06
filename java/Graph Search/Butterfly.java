
package student;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import danaus.*;

public class Butterfly extends AbstractButterfly {

  //Initialize Variables
  public Direction[] direction = Direction.values();
  public TileState[][] tile;
  public List<Aroma>[][] tileAroma;
  List<Direction>[][] way;
  ArrayList<TileState> priority = new ArrayList<TileState>();
  Boolean lie = false;

  //Records Properties of each Tile 
  public @Override TileState[][] learn() {
    tile = new TileState[getMapHeight()][getMapWidth()];
    dFS();
    return tile;
  }

 //Implements dFS to assign locations on map corresponding to a 2d array
  public void dFS(){

    refreshState();
    int row = state.location.row;
    int col = state.location.col;
    //assigns map state to tile
    tile[row][col] = state;

    //Iterates through possible directions of flight
    for(Direction d:Common.DIRECTIONS){

     /*Attempts to fly in direction d then recurses, Object collisions are caught. If you cannot fly in the direction
        Once all the path are exausted in the direction the butterfly goes back to it's initial tile and fly in another Direction.
      */
      try{
        if(tile[Common.mod(row+d.dRow, getMapHeight())][Common.mod(col+d.dCol, getMapWidth())] == null){
          fly(d,Speed.FAST);
          dFS();
          fly(Direction.opposite(d),Speed.FAST);
          refreshState();
        }
      }
      catch(ObstacleCollisionException c){
        tile[Common.mod((row+d.dRow),getMapHeight())][Common.mod((col + d.dCol),getMapWidth())] = TileState.nil;
      }
    }
  }

  /*Determines the closest flower and returns it's Aroma.
  */

  public Long  maxAroma(List<Long> flowerIds ){
    List<Aroma> aromas = state.getAromas();
    
    Aroma max = null;

    for(Aroma a: aromas){ 
      if(flowerIds.contains(a.getFlowerId()) == false){
        continue;
      }
      else if(max == null || a.intensity>max.intensity ){
        max = a;
      }
    }
    return max;
  }

  

  //collects previosly known flowers
  public void runDyus(List<Long> flowerIds){
    TileState lock;
    List<Aroma> aromas = state.getAromas();

    // loop that executes if Id is left to be found 
    while(flowerIds.size()>0){
      // finds closest flower based on flower intensity
      Aroma max = null;
      maxAroma = maxAroma(aromas);

      //breaks while loop if all leaned Id's are exhausted
      if(max == null){
        break;
      } 

      //uses variation of djakstra's to find minimum path to closest aroma (max)
      lock = dyus(max);

      for(Direction d: way[lock.location.row][lock.location.col]){
        fly(d,Speed.FAST);
      }
      refreshState();
      
      //checks and collects flower on tile
      for(int k =0; k<state.getFlowers().size(); k++){
        if(max.getFlowerId()==state.getFlowers().get(k).getFlowerId()){
          collect(state.getFlowers().get(k));
        }
      }

      aromas.remove(max);
      flowerIds.remove(max.getFlowerId());
    }
  }

  //Collects floweres spontaniously spawned
  public void runGeneral(List<Long> flowerIds){
    while(flowerIds.size()>0){ 
      refreshState();
      aromas = state.getAromas();
      Aroma max = null;

      // gets most intense aroma 
      max = maxAroma(flowerIds);

      regMove(max,0);
      aromas.remove(max);
      flowerIds.remove(max.getFlowerId());   
    }
  }

 //collects flowers on board List flowerIds, Collects learned flowers followed by newly spawned
  public @Override void run(List<Long> flowerIds) {
    TileState lock;
    List<Aroma> aromas = state.getAromas();

    //collects flowers on previosly board
    runDyus(flowerIds);
    //collects newly spawned flowers
    runGeneral(flowerIds);
  }




 //takes in parameter ID and returns it's index
 public int index(long id){

  for(int i =0; i< state.getAromas().size() ;i++){
   if(id == state.getAromas().get(i).getFlowerId()){
    return i;
   }
  }
  return -1;
 }

 /**
  * Fly to the tile that contains the flower given by max and collect the flower
  * state of the bfly is not refreshed
  * 
  * */
  public void regMove(Aroma max, int start){
    refreshState();
    Direction[] theDir = Common.DIRECTIONS;
    int endTime = 0;

    //find index of next flower to collect
    int indx = index(max.getFlowerId());
    TileState pastState = state;

    //Collects the flower
    for(Flower f : state.getFlowers()){
      if(max.getFlowerId()== f.getFlowerId()){
        collect(f);
        return;
      }
    }

    int i = start;
    // checks if there is a direction to traverse
    while( i < theDir.length){

      int row = Common.mod(state.location.row + theDir[i].dRow, getMapHeight());
      int col = Common.mod(state.location.col + theDir[i].dCol, getMapWidth());
      TileState states = tile[row][col];

      // flies to tile if tile is flyable
      if(states != TileState.nil && states != state ){
        fly(theDir[i],Speed.FAST);
        refreshState();

        //calls recursively if intensity if greater than prior tile, else returns to tile
        if(state.getAromas().get(indx).intensity > pastState.getAromas().get(indx).intensity){
          regMove(max,i);
          return;
        }
        else{
          fly(Direction.opposite(theDir[i]),Speed.FAST);
          refreshState();
        }
      }
      
      i++;
      if(i == theDir.length){
        i = 0;
      }
    }
  }

 
 /*
  * Assigns minimum distance to the corresponding tile in an array
  * Returns the value of the tile containing the max aroma
  */
  public TileState dyus( Aroma max ){
  
    way = new ArrayList[getMapHeight()][getMapWidth()];

    TileState states;
    TileState curState = state;
    way[state.location.row][state.location.col] = new ArrayList<Direction>();
    priority.add(curState);
    TileState lock = null;
    int indx = index(max.getFlowerId());

    ArrayList<TileState> done = new ArrayList<TileState>();

    // While there is a tile to be accounted for, continues to find minimum path
    while(priority.size() != 0){
      curState = priority.get(0);

      // finds if flower is on tile
      for(int k = 0; k < curState.getFlowers().size() ; k++){
        if(max.getFlowerId()==curState.getFlowers().get(k).getFlowerId()){
        lock = curState;
        }
      }
     
     // Traverses all possible directions from tile
      for(Direction d:Common.DIRECTIONS){
        int i = 0;
      
        int row = Common.mod(curState.location.row +d.dRow, getMapHeight());
        int col = Common.mod(curState.location.col +d.dCol, getMapWidth());
        states = tile[row][col];

        // check if tile is traverable
        if(states.type == TileType.LAND|| states.type == TileType.FOREST && states != state){ 
          //if tile has no shorted direction assigned or a new shorted direction is possible, spot is overridden
          if(way[row][col] == null || way[row][col].size()> (way[curState.location.row][curState.location.col].size()+1) ){
            way[row][col] = new ArrayList<Direction>(way[curState.location.row][curState.location.col]);
            way[row][col].add(d);
          }
       
          // checks if tile has been traversed if
          if(done.contains(states)==false){
            //Places tiles in order of intensity
            if(states.getAromas().get(indx).intensity <= priority.get(priority.size()-1).getAromas().get(indx).intensity){
              priority.add(states);
            }
            else{
              while(states.getAromas().get(indx).intensity <= priority.get(i).getAromas().get(indx).intensity){
                i++;
              }
            priority.add(i, states);
            }
            done.add(states);     
          }
        }
      }

      done.add(priority.get(priority.indexOf(curState)));
      priority.remove(curState);
    } 
    return lock;
  }
 
}




