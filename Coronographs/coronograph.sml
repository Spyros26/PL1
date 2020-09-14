(*http://homepages.inf.ed.ac.uk/mfourman/teaching/mlCourse/notes/L13.html%5B.htm%5D (used for member function and helped for creating the graph as (vertices,edges):(int list, (int*int) list))*)

(*coronograph "C:/graphs.txt"
coronograph "C:/Users/spval/Documents/sources.cm/graphs.txt";
use"C:/Users/spval/Documents/PL1/coronograph.sml"; *)


fun coronograph (infile: string) =
	
	let
		val fd = TextIO.openIn infile
						
		fun int_from_stream stream =
  			 Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream);

		val T = int_from_stream fd
		
		
		fun len(xs) =
		    case xs of
			[] => 0
		      | (_::xs') => 1 + len(xs')


		fun vertexlist (counter3,l) = 
			if counter3 > 1 then
				vertexlist (counter3-1, counter3::l)
			else 1::l

		fun edgelist (a, b, counter2, l) =
			if (counter2) > 1 then (
				Array.update(l,(a-1),b::(Array.sub(l,a-1)));
           			Array.update(l,(b-1),a::(Array.sub(l,b-1)));
				edgelist (int_from_stream fd, int_from_stream fd, (counter2)-1, l))
			else (
				Array.update(l,(a-1),b::(Array.sub(l,a-1)));
          			Array.update(l,(b-1),a::(Array.sub(l,b-1)));
				l  
				)



		fun is_corona (vertices, edges: int list array, M) =
			let

				val visited2 = BitArray.array (len(vertices),false);
				val cyclicstart = BitArray.array (len(vertices),false);
				val cyclicpart = BitArray.array (len(vertices),false)
	  

				fun mark_circle(l) =
				if l <> [] then (
					if (BitArray.sub (cyclicstart,((hd l)-1)) = true) then 
		  				BitArray.update (cyclicpart,(hd l)-1, true)
	    				else (
						BitArray.update (cyclicpart,(hd l)-1,true);
						mark_circle(tl l)
						))
				else ()
		     
				fun circle (v , vparent2, point , n) =
					if n <> [] then (
						if (BitArray.sub (visited2,((hd n)-1)) = false) then (
								BitArray.update (visited2,(hd n)-1,true);
								circle (hd n , v::vparent2, (tl n)::point, Array.sub(edges,(hd n)-1)))
						else
							(
							if  (hd n) <> (hd vparent2) then
							(
								BitArray.update (cyclicpart,v-1,true);
								BitArray.update (cyclicstart,(hd n)-1,true);
								mark_circle(vparent2)
							)
							else circle (v , vparent2, point, tl n)
							))
					else  (
						case vparent2 of
                              				(x::xs) =>  circle (hd vparent2 , tl vparent2 ,(tl point),(hd point))
               						| [] => mark_circle(vparent2) 
						)
				
				fun trees (vertices,edges: int list array) =
					let
						val visited3 = BitArray.array (len(vertices),false);
	
						fun sum(xs) =
							case xs of
								[] => 0
	 							| (x::xs') => x + sum(xs')

						(* Sort a list of integers. *)
						fun mergeSort nil = nil
	  					| mergeSort [e] = [e]
	  					| mergeSort theList =
	    						let
								(* From the given list make a pair of lists * (x,y), where half the elements of the * original are in x and half are in y. *)
									fun halve nil = (nil, nil)
		 							 | halve [a] = ([a], nil)
		  							| halve (a::b::cs) =
		    								let
											val (x, y)= halve cs
				    						in
											(a::x, b::y)
		    								end;
						(* Merge two sorted lists of integers into * a single sorted list. *)
									fun merge (nil, ys) = ys
		 							 | merge (xs, nil) = xs
		  							| merge (x::xs, y::ys) =
		   								 if x < y then x :: merge(xs, y::ys)
		   								 else y :: merge(x::xs, ys);
									val (x, y) = halve theList
	    						in
								merge (mergeSort x, mergeSort y)
	    						end

						fun get_tree (v , n, comps ,point, vparent3 ) =
							if (n <> []) then (
			    					
			    					if ((BitArray.sub (visited3,((hd n)-1)) = false) andalso (BitArray.sub (cyclicpart,((hd n)-1)) = false)) then
									(BitArray.update (visited3,(hd n)-1,true);
									get_tree (hd n, (Array.sub(edges,(hd n)-1)), (hd n) :: comps,(tl n)::point, v::vparent3))
								else get_tree (v, tl n, comps,point, vparent3) )
							else (
								case vparent3 of
									[] => comps
									|(x::xs) => get_tree (x, hd point, comps, tl point, xs) 
							)
	  
	
						fun printer (onelist) =
							case onelist of
								[] => print ("\n")
								|[x] => print (Int.toString(x) ^ "\n")
								|(x::xs) => (
									print (Int.toString(x) ^ " ");
		   							printer(xs)
	       								)	
	 
	
        					fun search_tree (v, lim, forest) =
							if ((v-1) < lim) then
				    			(
	    							if (BitArray.sub (cyclicpart,(v-1)) = true) then (
									search_tree (v+1 , lim , len(get_tree(v, Array.sub(edges,v-1), (v::[]),[], []))::forest))
								else search_tree (v+1 , lim , forest)
		    					)
							else mergeSort forest 

						fun check_and_print (x) =
							if (sum(x) = len(vertices)) then (
								print("CORONA ");
								print (Int.toString(len(BitArray.getBits cyclicpart)));
	     							print ("\n");
								printer (x) )
							else print ("NO CORONA\n") 

    					in
						check_and_print (search_tree(1, len(vertices), []))
	       
    					end
				
				fun foo (vertices, edges, M) =
					if len(vertices) <> M then print("NO CORONA\n")
					else (BitArray.update(visited2,0,true); circle(1 , [] , [] , Array.sub(edges,0)); trees(vertices,edges))
	       
    			in
				foo(vertices, edges,M)	
			end
	       	
		

		fun helper (N, M, counter) =
			if counter > 1 then
				(is_corona (vertexlist(N,[]), edgelist(int_from_stream fd, int_from_stream fd, M, Array.array(N,[])), M); helper (int_from_stream fd, int_from_stream fd, counter-1))
			else (is_corona (vertexlist(N,[]), edgelist(int_from_stream fd, int_from_stream fd, M, Array.array(N,[])), M); TextIO.closeIn fd)
		

	in
		helper (int_from_stream fd, int_from_stream fd, T)
	end;


    				 


