# Esta funcion se encarga de establecer la informacion de los problemas
# y cargar el script correspondiente a la funcion
#
# Los argumentos que recibe son:
#
# problema:                   el nombre del problema 
#
# deveuelve una lista con la informacion del problema, la descripcion la podeis
# consultar en :
# http://www.sfu.ca/ssurjano/stybtang.html
#
# Ademas en la parte final de la funcion se realiza un source() del fichero que contiene
# la definicien de la funcion a optimizar (maximizar), dicha fichero contiene siempre
# una funcion llamada evaluadora(), por lo que en genetico() no es necesario
# dar el nombre explicito de la funcion a resolver, basta con llamar a evaluadora()

inicializador <- function(problema) {
  info <- NULL
  switch(problema,      
         Ackley =      {info$n <- 10;  info$l <- rep(-32.768,info$n); info$u <- -info$l;},
         Bukin6 =      {info$n <-  2;  info$l <- c(-15,-3);           info$u <- c(-5,3);},
         CrossInTray = {info$n <-  2;  info$l <- c(-10,-10);          info$u <- -info$l;},
         DropWave =    {info$n <-  2;  info$l <- rep(-5.12,info$n);   info$u <- -info$l;},
         Eggholder =   {info$n <-  2;  info$l <- rep(-512,info$n);    info$u <- -info$l;},
         GramacyLee12= {info$n <-  1;  info$l <- rep(0.5,info$n);     info$u <- rep(2.5,info$n);},
         Griewank    = {info$n <-  5;  info$l <- rep(-600,info$n);    info$u <- -info$l;},
         HolderTable=  {info$n <-  2;  info$l <- rep(-10,info$n);     info$u <- -info$l;},
         Langermann =  {info$n <-  2;  info$l <- rep(0,info$n);       info$u <- rep(10,info$n);},
         Levy =        {info$n <-  5;  info$l <- rep(-10,info$n);     info$u <- -info$l;},
         Levy13 =      {info$n <-  2;  info$l <- rep(-10,info$n);     info$u <- -info$l;},
         Rastrigin =   {info$n <- 10;  info$l <- rep(-5.12,info$n);   info$u <- -info$l;},
         Schaffer2 =   {info$n <-  2;  info$l <- rep(-100,info$n);    info$u <- -info$l;},
         Schaffer4 =   {info$n <-  2;  info$l <- rep(-100,info$n);    info$u <- -info$l;},
         Schwefel =    {info$n <-  5;  info$l <- rep(-500,info$n);    info$u <- -info$l;},
         Shubert =     {info$n <-  2;  info$l <- rep(-5.12,info$n);   info$u <- -info$l;},
         Bohachevsky1=  {info$n <-  2;  info$l <- rep(-100,info$n);   info$u <- -info$l;},
         Bohachevsky2=  {info$n <-  2;  info$l <- rep(-100,info$n);   info$u <- -info$l;},
         Bohachevsky3=  {info$n <-  2;  info$l <- rep(-100,info$n);   info$u <- -info$l;},
         Perm0db=       {info$n <-  5;  info$l <- rep(-info$n,info$n);info$u <- -info$l;},
         Rothyp=        {info$n <-  5;  info$l <- rep(-65.536,info$n);info$u <- -info$l;},
         Sphere=        {info$n <-  5;  info$l <- rep(-5.12,info$n);  info$u <- -info$l;},
         Sphere2=       {info$n <-  6;  info$l <- rep(0,info$n);      info$u <- rep(1,info$n);},
         Sumpow=        {info$n <-  10; info$l <- rep(-1,info$n);     info$u <- -info$l;},
         Sumsqu=        {info$n <-  10; info$l <- rep(-10,info$n);    info$u <- -info$l;},
         Trid=          {info$n <-  6;  info$l <- rep(-info$n^2,info$n);   info$u <- -info$l;},
         Booth=         {info$n <-  2;  info$l <- rep(-10,info$n);    info$u <- -info$l;},
         Matyas=        {info$n <-  2;  info$l <- rep(-10,info$n);    info$u <- -info$l;},
         Mccormick=     {info$n <-  2;  info$l <- c(-1.5,-3);         info$u <- c(4,4);},
         Powersum=      {info$n <-  4;  info$l <- rep(0,info$n);      info$u <- rep(info$n,info$n);},
         Zakharov =     {info$n <-  2;  info$l <- rep(-5,info$n);     info$u <-rep(10,info$n);},
         Camel3 =       {info$n <-  2;  info$l <- rep(-5,info$n);     info$u <- -info$l;},
         Camel6 =       {info$n <-  2;  info$l <- c(-3,-2);           info$u <- -info$l;},
         DixonPrice =   {info$n <-  5;  info$l <- rep(-10,info$n);    info$u <- -info$l;},
         Rosenbrock1 =  {info$n <-  5; info$l <- rep(-5,info$n);      info$u <- rep(10,info$n);},
         Rosenbrock2 =  {info$n <-   4; info$l <- rep(0,info$n);      info$u <- rep(1,info$n);},
         DeJong5 =      {info$n <-   2; info$l <- rep(-65.536,info$n);info$u <- -info$l;},
         Easom =        {info$n <-   2; info$l <- rep(-100,info$n);   info$u <- -info$l;},
         Michalewicz =  {info$n <-   10; info$l <- rep(0,info$n);      info$u <- rep(pi,info$n);},
         Beale =        {info$n <-    2; info$l <- rep(-4.5,info$n);   info$u <- rep(pi,info$n);},
         Branin1 =      {info$n <-    2; info$l <- c(-5,0);            info$u <- c(10,15);},
         Branin2 =      {info$n <-    2; info$l <- c(-5,0);            info$u <- c(10,15);},
         Branin3 =      {info$n <-    2; info$l <- c(-5,0);            info$u <- c(10,15);},
         Colville =     {info$n <-    4; info$l <- rep(-10,info$n);    info$u <- -info$l;},
         Forrester  =   {info$n <-    1; info$l <- rep(0,info$n);      info$u <- rep(1,info$n);},
         GoldsteinPrice={info$n <-    2; info$l <- rep(-2,info$n);     info$u <- -info$l;},
         Hartmann3D =   {info$n <-    3; info$l <- rep(0,info$n);      info$u <-  rep(1,info$n);},
         Hartmann4D =   {info$n <-    4; info$l <- rep(0,info$n);      info$u <-  rep(1,info$n);},
         Hartmann6D =   {info$n <-    6; info$l <- rep(0,info$n);      info$u <-  rep(1,info$n);},
         Permdb=        {info$n <-    2; info$l <- rep(-info$n,info$n);info$u <- -info$l;},
         Powell=        {info$n <-    5; info$l <- rep(-4,info$n);     info$u <-  rep(5,info$n)},
         Shekel=        {info$n <-    4; info$l <- rep(0,info$n);      info$u <- rep(10,info$n);},
         StyblinskiTang={info$n <-   15; info$l <- rep(-5,info$n);     info$u <- -info$l;}
  )

  source(paste("funciones/",problema,".R",sep=""))
  return(info)  
}
