let test_successor candidate =
	   (candidate (-1000) = ( -999))
   &&	(candidate 0 = 1)
	&& (candidate 1000 = 1001)
   && (candidate 98374 = 98375) ;;


let test_not candidate =
      (candidate true = false) 
   &&	(candidate false = true) ;;