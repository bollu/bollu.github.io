set Obj;
param price{i in Obj};
var useitem{i in Obj} binary;
minimize delta: 
    abs((sum{i in Obj} useitem[i]*price[i]) - 
        (sum{i in Obj} (1 - useitem[i]) * price[i]));
