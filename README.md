## Chapter 3

`Ch3.main` を実行すると、訓練済みニューラルネットで認識したテストデータが出力される。

データは 10,000 件あり、処理に数分程度かかる。

```
% stack runghc app/Ch3.hs > ch3.txt
% head -n 77 ch3.txt
                            
                            
                            
                            
                            
                            
                            
      -#*+:.                
      %@@@@@########*:      
      :=:=*%@%@@@@%@@+      
            : :::: @@=      
                  -@%       
                  @@-       
                 +@@.       
                :@@:        
                +@#         
                %@:         
               =@#          
              :@@:          
              %@*           
             #@%.           
            .@@-            
           .%@=             
           +@@:             
          :@@@:             
          =@@%.             
          =@%               
                            
0  0.00% 
1  0.00% 
2  0.02% 
3  0.01% 
4  0.00% 
5  0.00% 
6  0.00% 
7 99.92% *******************
8  0.00% 
9  0.05% 
                            
                            
                            
          ==*@@+-           
         *@@@@@@%.          
        *@@@%+*@@=          
       :@@%.   %@+          
       -@%    =@@:          
        .     %@@:          
             =@@#           
            :@@@:           
            +@@+            
           *@@*             
           @@@.             
          #@@+              
         -@@#               
         #@@+               
        +@@*                
        @@@                 
        @@@.         .++++  
        @@@@@@@@*+*@@@@@@@= 
        *@@@@@@@@@@@@@@*==: 
         ====*@@@*==.       
                            
                            
                            
                            
                            
0  0.04% 
1  0.11% 
2 98.59% *******************
3  0.65% 
4  0.00% 
5  0.07% 
6  0.51% 
7  0.00% 
8  0.03% 
9  0.00% 
                            
```
