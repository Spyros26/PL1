(*https://gist.github.com/edalorzo/4670775 (used for the sum, head, tail, take, len functions. The link also helped for creating the printil function.)
https://html.developreference.com/article/21318995/conversion+from+decimal+to+binary+in+SML (used for the reverse_binary function)
https://stackoverflow.com/questions/29809722/reading-an-integer-file-to-an-integer-list-in-sml (used for the int_from_stream function)*)

		


fun powers2 (infile: string) =
	let
		val fd = TextIO.openIn infile
		
		fun int_from_stream stream =
  			 Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream);

		val T = int_from_stream fd
		
		fun powers (N, K) =
			let
		
				fun sum x =
					case x of
						[] => 0
	 		     			| (x :: xs) => x + sum xs;

			
				fun reverse_binary Num = 
                		    	if Num = 0 then []
                    			else (Num mod 2) :: reverse_binary (Num div 2);
 	  			
				fun len(xs) =
							case xs of
								[] => 0
		 						| (_::xs') => 1 + len(xs');
		
				fun reshape x =
					let
						fun tail(xs) =
							case xs of
								[] => raise List.Empty
	  							| (_::xs') => xs';
		
						fun head(xs) =
							case xs of
								[] => raise List.Empty
				 				 | (x::_) => x;
		
						fun take(xs, n) =
							if n < 0 
							then raise Subscript
							else 
								case xs of 
									[] => []
		 			 				| (x::xs') => if n > 0 then x::take(xs',n-1) else [];


						fun second x =
							case x of
								[] => []
								|(x::xs) => if (head xs) > 0 then (x+2)::((head xs) -1)::(tail xs) else second xs;

						fun first x =
							take (x, len(x)-len(second x));
		
						fun connect x y = x @ y;
					in
						connect (first x) (second x)
					end	
		
				fun printil(x) = 
		
					let
						fun print1i(i) = (print(Int.toString i); true)
						fun print1s(s) = (print(s); true)
						fun printl(x,i) =
							if i = len x then (print1s("["); printl(x,i-1))
							else
								case x of
									[] => print1s("]\n")
  	      								| (h::t) => if i > 0 then (print1i(h) andalso print1s(",") andalso printl(t,i-1))
										else (print1i(h) andalso printl(t,i-1))
					in
						printl(x,len x)
					end

				fun compact x =
					case x of
						[] => []
						|(x::xs) => if (sum xs) = 0 then x :: compact [] else x :: compact xs;
		
				fun execute current_list k =
					if (sum current_list) = k then compact current_list
					else execute (reshape current_list) k;

		         	fun powers_2 a b =
					if (b < sum (reverse_binary a) orelse b > a) then []
					else execute (reverse_binary a) b;
		
			in
				Control.Print.printLength := 100;
				printil(powers_2 N K) 
			end
	
		fun helper (a, b, counter) =
			if counter > 1 then
				(powers (a, b); helper (int_from_stream fd, int_from_stream fd, counter-1))
			else (powers (a, b); TextIO.closeIn fd)
	in	
		helper (int_from_stream fd, int_from_stream fd, T)
	end;

	
