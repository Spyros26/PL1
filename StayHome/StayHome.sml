(*https://github.com/giannisdaras/ntua-lambda/commit/b1ff5709e4d605ab8df548346de95a79a3799fc6?fbclid=IwAR3UAMiyQcyeDBHTifrA_kW6LrjQBXmVa8Y8c7Fo5GfBgxTp3rP27C_KCEQ 
(helped for initializing grid, get_if_not_x, floodfill, references...)*)

fun stayhome infile = 
	let 
		fun get_first_tuple (x,y,z) = x;
		fun get_second_tuple (x,y,z) = y;
		fun get_third_tuple (x,y,z) = z;
		fun get_first_pair (x,y) = x;
		fun get_second_pair (x,y) = y;
		fun grid_initialization () = 
			let 
				fun strip a_string =
					let 
						val string_size=String.size(a_string);
					in 
						String.substring(a_string,0,string_size-1)
					end;

				fun get_padding 0 = nil 
				| get_padding (k):string list = "X" :: get_padding(k-1);

				fun readlist (infile : string) = 
					let 
						val fd = TextIO.openIn infile 
						fun loop fd = 
							case TextIO.inputLine fd of 
								SOME line => map (fn x => Char.toString(x)) (String.explode("X" ^ ( strip line ) ^ "X")) :: loop fd 
				    				| NONE => [] 
					in 
				  		loop fd before TextIO.closeIn fd 
					end ;

				val list_of_lists = readlist(infile);
				val N = length list_of_lists;
				val M = length (hd list_of_lists)-2;
				val padding = get_padding(M+2);
				val new_list = (padding :: list_of_lists) @ [padding];
			in 	
				(Array2.fromList(new_list) , N, M)
			end;

		val tuple = grid_initialization ();
		val grid = #1 tuple;
		val N = #2 tuple;
		val M = #3 tuple;

		(* Find where W is *)
		fun initialize_wuhan () =
			let
				val my_queue = nil;
				fun helper (i,j,my_queue) = 
					if (i <= N) then
						if (j <= M) then 
							if (Array2.sub(grid,i,j) = "W") then (i,j,0) :: my_queue
							else helper(i,j+1,my_queue)
						else helper(i+1,1,my_queue)
					else
						my_queue;
			in 
				helper(1,1,my_queue)
			end;

		val corona_time = ref ( initialize_wuhan () );

		(* Find where S is *)
		fun initialize_start () =
			let
				val my_queue = nil;
				fun helper (i,j,my_queue) = 
					if (i <= N) then
						if (j <= M) then 
							if (Array2.sub(grid,i,j) = "S") then (i,j,"V") :: my_queue
							else helper(i,j+1,my_queue)
						else helper(i+1,1,my_queue)
					else
						my_queue;
			in 
				helper(1,1,my_queue)
			end;

		val walk = ref ( initialize_start () );

		(* Find where T is *)
		fun initialize_end () =
			let
				val my_queue = nil;
				fun helper (i,j,my_queue) = 
					if (i <= N) then
						if (j <= M) then 
							if (Array2.sub(grid,i,j) = "T") then (i,j) :: my_queue 
							else helper(i,j+1,my_queue)
						else helper(i+1,1,my_queue)
					else
						my_queue;
			in 
				helper(1,1,my_queue)
			end;

		val walk_end = ref ( initialize_end () );
		
		(*Find the airports*)
		fun initialize_airports () =
			let
				val my_queue = nil;
				fun helper (i,j,my_queue) = 
					if (i <= N) then
						if (j <= M) then 
							if (Array2.sub(grid,i,j) = "A") then helper(i,j+1,(i,j) :: my_queue)
							else helper(i,j+1,my_queue)
						else helper(i+1,1,my_queue)
					else
						my_queue;
			in 
				helper(1,1,my_queue)
			end;

		val airports = ref (initialize_airports () );

		fun get_neighbors (i,j) =
				let 
					fun get_if_not_x (i,j)=
						let 
							val x = Array2.sub(grid,i,j)
						in
							if (x = "X") then nil
							else (i,j)::nil
						end;
				in 
					get_if_not_x(i+1,j) @ get_if_not_x(i,j-1) @ get_if_not_x(i,j+1) @ get_if_not_x(i-1,j)
				end;

		fun exists(e, xs) =
    			case xs of
        			[] => false
     				| x::xs' => x = e orelse exists(e, xs');

		(*Spread the virus*)
		fun covid (corona_time: (int * int * int) list ref, counter:int ref, ref false) = ()
		| covid (corona_time, counter: int ref,flag_queue) = 
			let 
				val corona_time_new = ref nil;

				fun helper nil = () 
				| helper corona_time =
					let 
						val current_i = get_first_tuple (hd corona_time);
						val current_j = get_second_tuple (hd corona_time);
						val current_time = get_third_tuple (hd corona_time);
						val neighbors = get_neighbors (current_i,current_j);
						fun run_on_neighbors (counter, nil) = ()
						| run_on_neighbors (counter,neighbors) = 
							let
								val neighbor_i = get_first_pair (hd neighbors);
								val neighbor_j = get_second_pair (hd neighbors);
								val neighbor_symbol = Array2.sub(grid,neighbor_i,neighbor_j);
								fun delete (item, list) =
    									case list of
    										[] => []
									      | xs::ys => if item = xs then delete(item,ys)
          										else xs::delete(item,ys)
								fun corona_air(nil,counter) = ()
								| corona_air(airports,counter) =
									let 
										val airport_i = get_first_pair (hd airports);
										val airport_j = get_second_pair (hd airports); 
									in
										corona_time_new := (airport_i,airport_j,!counter+5) :: !corona_time_new;
										Array2.update(grid,airport_i,airport_j,Int.toString(!counter+5));
										corona_air(tl airports,counter)
									end
							in 
								if(!counter = current_time + 2) then (
									if (neighbor_symbol = "." orelse neighbor_symbol = "S" orelse neighbor_symbol = "T") then (
										corona_time_new := (neighbor_i,neighbor_j,!counter) :: !corona_time_new;
										flag_queue := true;
										Array2.update(grid,neighbor_i,neighbor_j,Int.toString(!counter))
									)
									else if (neighbor_symbol = "A") then (
										flag_queue := true;
										corona_time_new := (neighbor_i,neighbor_j,!counter) :: !corona_time_new;
										Array2.update(grid,neighbor_i,neighbor_j,Int.toString(!counter));
										corona_air(delete((neighbor_i,neighbor_j),!airports),counter)
									)
									else ();
									run_on_neighbors(counter,tl neighbors)
								)
								else ()
								
							end;

					in 
						run_on_neighbors(counter, neighbors);
						helper (tl corona_time)
					end;

				
			in 
				counter := !counter + 1;
				flag_queue := false;
				helper (!corona_time);
				if (((!counter) mod 2 = 1 andalso !flag_queue = false) orelse (!flag_queue = false andalso length(!corona_time) <> 0)) then flag_queue := true
				else();
				covid(ref((!corona_time_new)@(List.filter (fn (x,y,z) => z >= !counter - 1 ) (!corona_time))),counter,flag_queue)
			end;

		
		(*Go home*)
		val path = ref nil;
		fun corona_odyssey (walk: (int * int * string) list ref, path: (int * int) list list ref, counter2:int ref, ref true, flag_queue2) = ()
		| corona_odyssey (walk, path, counter2, flag_end2, ref false) = ()
		| corona_odyssey (walk, path, counter2, flag_end2, flag_queue2) = 
			let 
				val walk_new = ref nil;
				val path_new = ref nil;
				fun reverse2(xs) =
					let
						fun aux(xs, acc) =
							case xs of
								[] => acc
								| (x::xs') => aux(xs', x :: acc)
					in
						aux(xs, [])	
					end
	
				fun helper (nil) = () 
				| helper (walk) =
					let 
						val current_i = get_first_tuple (hd walk);
						val current_j = get_second_tuple (hd walk);
						val current_time = get_third_tuple (hd walk);
						val neighbors = get_neighbors (current_i,current_j);
						
						fun run_on_neighbors (counter, nil) = ()
						| run_on_neighbors (counter,neighbors) = 
							let
								val neighbor_i = get_first_pair (hd neighbors);
								val neighbor_j = get_second_pair (hd neighbors);
								val neighbor_symbol = Array2.sub(grid,neighbor_i,neighbor_j);
								
		
								fun show_me_the_way(i,j) = 
									let 
										val rev_path = ref(reverse2(!path @ [[(i,j)]]));
										val my_way = ref ([(i,j)]);
										fun get_neighbors (i,j) =
											let 
												fun get_if_not_x (i,j)=
													let 
														val x = Array2.sub(grid,i,j)
													in
														if (x = "X") then nil
														else (i,j) :: nil
													end;
											in 
												get_if_not_x(i+1,j) @ get_if_not_x(i,j+1) @ get_if_not_x(i,j-1) @ get_if_not_x(i-1,j)
											end;

										fun help (nil) = nil
										| help ([x]) = !my_way 
										| help (rev_path) =
											let
												val current_i = get_first_pair (hd (!my_way));
												val current_j = get_second_pair (hd (!my_way));
												val neighbors = get_neighbors (current_i,current_j);
												
												fun find_path(rev_path,nil) = ()
												| find_path(rev_path,neighbors) = 
													let											
														val neighbor_i = get_first_pair (hd neighbors);
														val neighbor_j = get_second_pair (hd neighbors);
													in
														if (exists((neighbor_i,neighbor_j), hd(tl(rev_path)))) then (my_way := (neighbor_i,neighbor_j) :: !my_way )
														else (find_path(rev_path,tl neighbors))
															 	
													end
											in
												find_path(rev_path,neighbors);
												help(tl rev_path) 							
											end
		
										fun helper (nil) = ()
										| helper ([x]) = ()
										| helper (x::xs) = (
											if (get_first_pair(x) = get_first_pair(hd(xs))+1) then print("U")
											else if(get_first_pair(x) = get_first_pair(hd(xs))-1) then print("D")
											else if(get_second_pair(x) = get_second_pair(hd(xs))-1) then print("R")
											else if(get_second_pair(x) = get_second_pair(hd(xs))+1) then print("L")
											else ();
											helper (xs)		
										) 
									in 
										print(Int.toString(!counter2)^"\n");
										helper(help(!rev_path) @ (!walk_end));
										print("\n")
									end;

							in 
								if (neighbor_symbol = "V" orelse neighbor_symbol = "W") then ()
								else (
									if (neighbor_symbol = "." orelse (neighbor_i,neighbor_j) <> hd(!walk_end) andalso (String.size(neighbor_symbol) > String.size(Int.toString(!counter2)) orelse neighbor_symbol > Int.toString(!counter2)))  then (
										walk_new := (neighbor_i,neighbor_j,"V") :: !walk_new;
										Array2.update(grid,neighbor_i,neighbor_j,"V");
										path_new := (current_i,current_j) :: !path_new;
										flag_queue2 := true			
									)
									else if ((neighbor_i,neighbor_j) = hd(!walk_end) andalso (String.size(neighbor_symbol) > String.size(Int.toString(!counter2)) orelse neighbor_symbol > Int.toString(!counter2))) then (
										walk_new := (neighbor_i,neighbor_j,"V") :: !walk_new;
										Array2.update(grid,neighbor_i,neighbor_j,"V");
										path_new := (current_i,current_j) :: !path_new;
										flag_end2 := true;
										show_me_the_way(current_i,current_j)			
									)
									else()
								);
								run_on_neighbors(counter2,tl neighbors)
									
							end;

					in 
						
						run_on_neighbors(counter2, neighbors);
						helper (tl walk)
					end;
		
			in 
				counter2 := !counter2 + 1;
				flag_queue2 := false;
				helper (!walk);
				corona_odyssey(ref(reverse2(!walk_new)),ref(!path @ [!path_new]),counter2,flag_end2,flag_queue2)
			end;

		
		
		val counter = ref 0;
		val flag_queue = ref true;
		val counter2 = ref 0;
		val flag_queue2 = ref true;
		val flag_end2 = ref false;
		
	in 
		covid (corona_time,counter,flag_queue);
		corona_odyssey(walk,path,counter2,flag_end2,flag_queue2);
		if (!flag_end2 = false) then print ("IMPOSSIBLE\n")
		else ()
	end;

(*use "C:/Users/spval/Documents/PL1/stayhome.sml"
stayhome "C:/Users/spval/Documents/PL1/d4.txt"*)
