library(shiny)
#input$size : サイズ, input$mu : 真の平均, input$sigma : 真の分散, input$seed : シードの値

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    set.seed(input$seed)
    rnorm(input$size, mean=input$mu, sd=sqrt(input$sigma)) # shiny のなかでデータを保持しておく.
  })
  
  output$Data <- renderTable({
    head_data <- head(dataInput()) ; tail_data <- tail(dataInput())
    dat_1 <- data.frame(head_data, tail_data) # DT パッケージでさらにインタラクティブになるが, DT パッケージをインストールすらしていない人向けではないため共有することに向いていないと判断.
  })
  
  
  output$graph_1<-renderPlot({
    mean_x <- mean(dataInput()) ; sig_x <- var(dataInput())
      
    func <- function(x,y){ # 外部で mean_x <- -1 : sig_x <- 2 ; n<-10 みたいなデータの情報が必要！
      ey <- exp(y)
      ey^(-input$size)*exp(-1/(2*ey^2)*((input$size-1)*sig_x + input$size*(mean_x-x)^2)) } # 近似したい関数
    
    x <- seq(-3,3,length.out=1000) # x座標
    y <- seq(-3,3,length.out=1000) # y座標
    z <- outer(x,y,func)           # z座標
    
    contour(x, y, z,drawlabels=F,
            xlim=c(-input$xlim, input$xlim), ylim=c(-input$ylim, input$ylim), xlab="μ", ylab="logσ", col=2, main="Posterior distribution") # これで等高線を描く. drawlabels = F で等高線のレベルを書かないので美しい.
    
  })
  
  
  
  output$graph_2 <- renderPlot({
    mean_x <- mean(dataInput()) ; sig_x <- var(dataInput())
    mu_1 <- mean_x ; sig_1 <- ((input$size-1)/input$size)*sig_x # n で割った分散がsig_x
    mu_2 <- log(sqrt(sig_1)) ; sig_2 <- 1/(2*input$size)
    
    func <- function(x,y) {
      nx <- (x-mu_1)/(sig_1/sqrt(input$size)) ; ny <- (y-mu_2)/sqrt((sig_2)) # 標準化
      1/(2*pi*sqrt(sig_1*sig_2))*exp(-nx^2-ny^2) } # 無相関な2次元正規分布 : 実は 2*pi を書かなくても出力は同じになる.
        
    x <- seq(-3,3,length.out=1000) # x座標
    y <- seq(-3,3,length.out=1000) # y座標
    z <- outer(x,y,func)           # z座標
    
    contour(x, y, z,drawlabels=F,
            xlim=c(-input$xlim, input$xlim), ylim=c(-input$ylim, input$ylim),xlab="μ", ylab="logσ", col=4, main="Normal distribution") # これで等高線を描く. drawlabels = F で等高線のレベルを書かないので美しい.
    
  })
  
  
  output$graph_3 <- renderPlot({
    mean_x <- mean(dataInput()) ; sig_x <- var(dataInput())
    
    ## graph_1 のコピペ(始)
    
    func <- function(x,y){ # 外部で mean_x <- -1 : sig_x <- 2 ; n<-10 みたいなデータの情報が必要！
      ey <- exp(y)
      ey^(-input$size)*exp(-1/(2*ey^2)*((input$size-1)*sig_x + input$size*(mean_x-x)^2)) } # 近似したい関数
    
    x <- seq(-3,3,length.out=1000) # x座標
    y <- seq(-3,3,length.out=1000) # y座標
    z <- outer(x,y,func)           # z座標
    
    contour(x, y, z,drawlabels=F,
            xlim=c(-input$xlim, input$xlim), ylim=c(-input$ylim, input$ylim), col=2) # これで等高線を描く. drawlabels = F で等高線のレベルを書かないので美しい.
    
    ## graph_1 のコピペ(終)
    
    par(new=T)
    
    ## graph_2 のコピペ(始)
    
    mu_1 <- mean_x ; sig_1 <- ((input$size-1)/input$size)*sig_x # n で割った分散がsig_x
    mu_2 <- log(sqrt(sig_1)) ; sig_2 <- 1/(2*input$size)
    
    func <- function(x,y) {
      nx <- (x-mu_1)/(sig_1/sqrt(input$size)) ; ny <- (y-mu_2)/sqrt((sig_2)) # 標準化
      1/(2*pi*sqrt(sig_1*sig_2))*exp(-nx^2-ny^2) } # 無相関な2次元正規分布 : 実は 2*pi を書かなくても出力は同じになる.
    
    x <- seq(-3,3,length.out=1000) # x座標
    y <- seq(-3,3,length.out=1000) # y座標
    z <- outer(x,y,func)           # z座標
    
    contour(x, y, z,drawlabels=F, 
            xlim=c(-input$xlim, input$xlim), ylim=c(-input$ylim, input$ylim), xlab="μ", ylab="logσ", col=4, main="Posterior ~ Normal") # これで等高線を描く. drawlabels = F で等高線のレベルを書かないので美しい.
    
    ## graph_2 のコピペ(終)
    
    ## あとは legend をつけるだけ
    
    legend("topright", legend = c("Posterior", "Normal"), col = c(2,4), lty=1, # legend と col は c() で複数選択可能. 別で用意して代入することも可.
           box.lwd = 2,             # 枠線の太さ
           box.lty = 1,             # 枠線のタイプ(実船・破線になったりする)
           box.col = "darkgreen",   # 枠線の色
           text.col = c(2,4),       # 凡例の文字色
           text.font = 2,           # 1=通常, 2=ボールド, 3=イタリック
           text.width = 0.115,      # テキストの幅
           bg = "white")            # 凡例領域の背景
    
    ## 所感 : 近似具合は見えるけど, スライダーの設定によってははみ出るので使いにくさが出てくるかも...(汗)
    ##        まぁでもそうすることで軸の値を気にしなくなるという悪い癖は生じなくなるだろうし, 平均や分散のスライダーで分布が動くところも shiny の楽しいところだと思います.
    
  })
})