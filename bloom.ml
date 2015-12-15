let getBloomParams double p double items =
    let slots  =  int (ceil ((items *. log p )/.((log 2.0)**2.0 ))) in
    let hashes =int( ceil ( (float slots *. log 2.0) /. items)) in
(slots,hashes)
