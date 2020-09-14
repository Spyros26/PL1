import java.io.*;
import java.util.*;

public class StayHome{
        public static void main(String[] args){
        Game gameInst= new Game(args[0]);
        }
}

//Tuple
class Tuple{
        public int x,y,z;
        public Tuple(int x,int y,int z){
                this.x=x;
                this.y=y;
                this.z=z;
        }
}

//Pair
class Pair{
        public int x,y;
        public Pair(int x,int y){
                this.x=x;
                this.y=y;
        }
}


class Game{
        public String[][] grid = new String[1002][1002];
        public String fileName=null;
        public int N=0;
        public int M=0;
        public Game(String fileName){
                this.fileName=fileName;
                this.fileProcesser();
                this.covid();
                this.corona_odyssey();
        }
        public ArrayList<Tuple> corona_time=new ArrayList<>();
        public ArrayList<Pair> airports=new ArrayList<>();
        public ArrayList<Tuple> walk=new ArrayList<>();
        public ArrayList<Pair> walk_end=new ArrayList<>();

        //Spread the virus
        public void covid(){
                //Find Wuhan
                for (int i=1; i<=N; i++){
                        for (int j=1; j<=M; j++){
                                if (grid[i][j].equals("W")){
                                        corona_time.add(new Tuple(i,j,0));
                                }
                        }
                }
                //Find airports
                for (int i=1; i<=N; i++){
                        for (int j=1; j<=M; j++){
                                if (grid[i][j].equals("A")){
                                        airports.add(new Pair(i,j));
                                }
                        }
                }
                //Find Sotiris
                for (int i=1; i<=N; i++){
                        for (int j=1; j<=M; j++){
                                if (grid[i][j].equals("S")){
                                        walk.add(new Tuple(i,j,1));
                                }
                        }
                }
                //Find the end
                for (int i=1; i<=N; i++){
                        for (int j=1; j<=M; j++){
                                if (grid[i][j].equals("T")){
                                        walk_end.add(new Pair(i,j));
                                }
                        }
                }

                boolean flagQueue=true;
                int counter=0;
                while (flagQueue){
                        ArrayList<Tuple> copy_corona = new ArrayList<>();
                        for (int a=0; a<corona_time.size(); a++){
                                copy_corona.add(new Tuple(0,0,0));
                        }
                        Collections.copy(copy_corona,corona_time);
                        counter++;
                        ArrayList<Tuple> corona_time_new = new ArrayList<>();
                        flagQueue=false;
                        while (!corona_time.isEmpty()){
                                Tuple current = corona_time.get(0);
                                int i=current.x;
                                int j=current.y;
                                int current_time=current.z;
                                ArrayList<Pair> neighbors=getNeighbors(i,j);
                                while(!neighbors.isEmpty()){
                                        Pair temp=neighbors.get(0);
                                        int neighbor_x=temp.x;
                                        int neighbor_y=temp.y;
                                        String neighbor_symbol=grid[neighbor_x][neighbor_y];
                                        if (counter == current_time + 2){
                                                if (neighbor_symbol.equals(".") || neighbor_symbol.equals("S") || neighbor_symbol.equals("T")){
                                                        corona_time_new.add(new Tuple(neighbor_x,neighbor_y,counter));
                                                        grid[neighbor_x][neighbor_y]=String.valueOf(counter);
                                                        flagQueue=true;
                                                }
                                                else if (neighbor_symbol.equals("A")){
                                                        corona_time_new.add(new Tuple(neighbor_x,neighbor_y,counter));
                                                        grid[neighbor_x][neighbor_y]=String.valueOf(counter);
                                                        flagQueue=true;
                                                        while(!airports.isEmpty()){
                                                                Pair airport = airports.get(0);
                                                                int airport_x=airport.x;
                                                                int airport_y=airport.y;
                                                                if (neighbor_x==airport_x && neighbor_y==airport_y) {}
                                                                else{
                                                                        corona_time_new.add(new Tuple(airport_x,airport_y,counter+5));
                                                                        grid[airport_x][airport_y]=String.valueOf(counter+5);
                                                                }
                                                                airports.remove(0);
                                                        }
                                                }
                                        }
                                        neighbors.remove(0);
                                }
                                corona_time.remove(0);
                        }

                        if ((counter%2==1 && flagQueue==false) || (flagQueue==false && copy_corona.size()!=0)){
                                flagQueue=true;
                        }

                        while (!copy_corona.isEmpty()){
                                Tuple temp = copy_corona.get(0);
                                if (temp.z >= counter-1){
                                        corona_time_new.add(new Tuple(temp.x,temp.y,temp.z));
                                }
                                copy_corona.remove(0);
                        }
                        corona_time = corona_time_new;

                }
        }

        //Go Home
        public void corona_odyssey(){
                boolean flagQueue2=true;
                boolean flagEnd2=false;
                int counter2=0;
                ArrayList<ArrayList<Pair>> path = new ArrayList<>();
                while (flagQueue2 && !flagEnd2){
                        counter2++;
                        ArrayList<Tuple> walk_new = new ArrayList<>();
                        ArrayList<Pair> path_new = new ArrayList<>();
                        flagQueue2=false;
                        while (!walk.isEmpty()){
                                Tuple current = walk.get(0);
                                int i=current.x;
                                int j=current.y;
                                int current_time=current.z;
                                ArrayList<Pair> neighbors=getNeighbors(i,j);
                                while(!neighbors.isEmpty()){
                                        Pair temp=neighbors.get(0);
                                        int neighbor_x=temp.x;
                                        int neighbor_y=temp.y;
                                        String neighbor_symbol=grid[neighbor_x][neighbor_y];
                                        if (neighbor_symbol.equals("V") || neighbor_symbol.equals("W")){}
                                        else {
                                                if (neighbor_symbol.equals(".") || (i!=walk_end.get(0).x || j!=walk_end.get(0).y) && (neighbor_symbol.length() > String.valueOf(counter2).length() || Integer.parseInt(neighbor_symbol) > counter2)) {
                                                        walk_new.add(0,new Tuple(neighbor_x,neighbor_y,1));
                                                        grid[neighbor_x][neighbor_y]="V";
                                                        path_new.add(0,new Pair(i,j));
                                                        flagQueue2=true;
                                                }
                                                if ((neighbor_x==walk_end.get(0).x && neighbor_y==walk_end.get(0).y) && (neighbor_symbol.length() > String.valueOf(counter2).length() || Integer.parseInt(neighbor_symbol) > counter2)){
                                                        walk_new.add(0,new Tuple(neighbor_x,neighbor_y,1));
                                                        grid[neighbor_x][neighbor_y]="V";
                                                        path_new.add(0,new Pair(i,j));
                                                        flagEnd2 = true;

                                                        System.out.println(counter2);
                                                        ArrayList<Pair> my_way = new ArrayList<>();
                                                        my_way.add(new Pair(i,j));
                                                        path.add(my_way);
                                                        Collections.reverse(path);
                                                        while(path.size()!=1){
                                                                Pair sth = my_way.get(0);
                                                                int current_i = sth.x;
                                                                int current_j = sth.y;
                                                                ArrayList<Pair> neighbors2=getNeighbors2(current_i,current_j);
outerloop:
                                                                while(!neighbors2.isEmpty()){
                                                                        Pair temp2=neighbors2.get(0);
                                                                        int neighbor2_x=temp2.x;
                                                                        int neighbor2_y=temp2.y;
                                                                        ArrayList<Pair> my_path = path.get(1);
                                                                        for (int k=0; k<my_path.size(); k++){
                                                                                if (my_path.get(k).x==neighbor2_x && my_path.get(k).y==neighbor2_y) {
                                                                                        my_way.add(0,temp2);
                                                                                        break outerloop;
                                                                                }
                                                                        }
                                                                        neighbors2.remove(0);
                                                                }
                                                                path.remove(0);
                                                        }
                                                        my_way.add(walk_end.get(0));
                                                        while(my_way.size()!=1){
                                                                Pair head = my_way.get(0);
                                                                Pair next = my_way.get(1);
                                                                int first = head.x;                                                                                                                                                     int second = head.y;                                                                                                                                                    int first_next = next.x;
                                                                int second_next = next.y;                                                                                                                                               if (first==first_next+1) System.out.print("U");
                                                                if (first==first_next-1) System.out.print("D");
                                                                if (second==second_next-1) System.out.print("R");
                                                                if (second==second_next+1) System.out.print("L");
                                                                my_way.remove(0);                                                                                                                                               }
                                                        System.out.println();
                                                }

                                        }
                                        neighbors.remove(0);
                                }
                                walk.remove(0);
                        }


                        path.add(path_new);
                        Collections.reverse(walk_new);
                        walk = walk_new;

                }

                if (flagEnd2==false)
                        System.out.println("IMPOSSIBLE");
        }

        //Read file, create grid
        public void fileProcesser(){
                String line = null;
                try{
                        FileReader fileReader = new FileReader(fileName);
                        BufferedReader bufferedReader = new BufferedReader(fileReader);
                        while((line = bufferedReader.readLine()) != null) {
                                if (N==0){
                                        this.M=line.length();
                                }
                                for (int j=1; j<=this.M; j++){
                                        grid[N+1][j]=String.valueOf(line.charAt(j-1));
                                }
                                N++;
                        }
                        this.N=N;                                                                                                                                                               bufferedReader.close();
                        for (int j=0; j<=M+1; j++){
                                grid[0][j]="X";                                                                                                                                                         grid[N+1][j]="X";
                        }
                        for (int i=0; i<=N+1; i++){
                                grid[i][0]="X";
                                grid[i][M+1]="X";
                        }
                }
                catch(FileNotFoundException ex) {
                        return;
                }
                catch(IOException ex) {
                        return;
                }
        }

        //get neighbors (in two different ways)
        public ArrayList<Pair> getNeighbors(int i,int j){
                ArrayList<Pair> g=new ArrayList<>();
                if (!grid[i+1][j].equals("X")){
                        g.add(new Pair(i+1,j));
                }
                if (!grid[i][j-1].equals("X")){
                        g.add(new Pair(i,j-1));
                }
                if (!grid[i][j+1].equals("X")){
                        g.add(new Pair(i,j+1));
                }
                if (!grid[i-1][j].equals("X")){
                        g.add(new Pair(i-1,j));
                }
                return g;
        }

        public ArrayList<Pair> getNeighbors2(int i,int j){
                ArrayList<Pair> g2=new ArrayList<>();
                if (!grid[i+1][j].equals("X")){
                        g2.add(new Pair(i+1,j));
                }
                if (!grid[i][j+1].equals("X")){
                        g2.add(new Pair(i,j+1));
                }
                if (!grid[i][j-1].equals("X")){
                        g2.add(new Pair(i,j-1));
                }
                if (!grid[i-1][j].equals("X")){
                        g2.add(new Pair(i-1,j));
                }
                return g2;
        }
}