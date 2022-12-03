library(shiny)
library(shinyjs)
library(shinyalert)
library(DT)
library(shinyWidgets)


ui <- fluidPage(
    tags$head(HTML("<title>iSAMBA</title>")),
    tags$head(tags$link(rel = "shortcut icon", href = "fav.png")),
    useShinyjs(),
    useShinyalert(),

    fluidRow(
        tags$img(id = "header", src = "iSAMBA.png", width = "100%"),
        tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-124289947-3'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-124289947-3');
</script>
")),
        fluidRow(
            id = "main_contents",
            tags$head(tags$style(type="text/css", "body {max-width: 1000px; text-align: center; margin: auto; padding: 5px 5px 20px 5px;}")),
            tags$head((tags$style(type="text/css", "#main_contents{box-shadow: 10px 10px 5px grey; border: 2px solid #73AD21; border-radius: 25px;
                                padding: 20px; margin: 10px 0px 0px 0px;}"))),
            tags$h1("iSAMBA: Super Areagram of Multiple Biogeographical Ambiguities
"),
            tags$br(),
            column(12,
                   fileInput("file1", "Choose File:", accept = c(".txt", ".tre", ".csv")),
                   tags$head(tags$style(type="text/css", ".form-group {margin: auto;}")),
                   hidden(tags$div(id="checking_msg","Checking input file ...")),
                   hidden(actionButton("bulid_superareagram", "Build Super Areagram")),

                   hidden(tags$div(id = "note", HTML("Note: Do not select any tree to consider all of them.<br />
                                                   Click on a tree to select it.
                                                   Click on a selected tree to unselect it.<br />"))),
                   hidden(actionButton("clearRows", "Clear Selection")),

                   tags$head(tags$style("#result{color: green;
                                        font-size: 20px;
                                        font-style: italic;
                                        padding: 10px 0px;
                                      }
                                      #note{color: red;
                                      padding: 20px 0px;}
                                      thead{display: none !important;}
                                      #DataTables_Table_0_length{padding-bottom: 20px !important;}")),
                   hidden(tags$img(id = "processing_gif",src = "processing.gif", width = "100px", height = "100px",
                                   style = "display: block;margin: auto;")),
                   hidden(tags$div(id = "result", "The SAMBA result is ready.")),
                   hidden(tags$div(id = "downloadsMsg", "Downloads:", style = "padding-bottom: 10px;")),
                   hidden(downloadButton("downloadInput", "The input Trees")),
                   hidden(downloadButton("downloadDataSS", "MRP-matrix .ss")),
                   hidden(downloadButton("downloadDataNEX", "MRP-matrix .nex")),
                   hidden(downloadButton("downloadSolved", "The Solved Trees")),


                   DT::dataTableOutput("treesTable"),
                   tags$head(tags$style(type="text/css", "p {font-size: 16px;} h2{text-decoration: underline;}
                                      #example{border: 1px solid #000; border-radius: 25px; padding: 10px; background-color: #eee;
                                      font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter', monospace;}")),
                   tags$h2("Tutorial"),
                   tags$p(HTML("iSAMBA tool accepts files with any extension that contain at least one areagram tree in parenthetical format.<br /><br />
                             Each areagram in the input file should end with a semicolon (;).<br /><br />
                             Example of the input file ( the '#' is used to represrnt the MAST case):<br /><br />
                             <div id='example'>(PA#NE#OR#AF#AFT#NT,(PA#NE#OR#NT,((PA#NE#OR#AF,ATM),(AN#ATM,NT))));<br />
                                            (NE,(PA#NE,(PA#NE#OR#AF#AFT#ATM,(PA#NE#OR#AFT#AF#ATM#AT#AN#NT,(NT#NE,AN#AF#AFT#OR#AT#ATM)))));<br />
                             </div>
                             ")),
                   tags$h2("Privacy Policy"),
                   tags$p(HTML("iSAMBA does not request any personal information as well as does not store the user’s input/output data.")),
                   tags$br(),
                   tags$a(href="mailto:muhsen.hammoud84@gmail.com", "Contact Us")
            )
        ),
        fluidRow(
          tags$br(),
          tags$br(),
          tags$div(id = "copyright",
                   HTML(paste0("@ ", format(Sys.Date(), "%Y"),
                               " <a href='http://www.ufabc.edu.br/' target='_blank'>Universidade Federal do ABC</a>")))
        )
    )

)



server <- function(input, output, session) {

    server_path <- if (dir.exists("/Users/muhsen/Google Drive/PhD Brazil/Research/isamba/")) "/Users/muhsen/Google Drive/PhD Brazil/Research/isamba/" else "/srv/shiny-server/isamba/"

    supermatrix_res_ss <- ""
    supermatrix_res_nex <- ""
    inputTrees <- ""
    ordered_samba_res_unique <- ""
    activities_log <- ""
    trees_global <- list()
    proxy = dataTableProxy('treesTable')

    observeEvent(input$clearRows, {
        proxy %>% selectRows(NULL)
    })


    observeEvent(input$file1, {

        hide("checking_msg")
        hide("processing_gif")
        hide("bulid_superareagram")
        hide("note")
        hide("clearRows")
        hide("pBarColumn")
        hide("downloadDataSS")
        hide("downloadDataNEX")
        hide("downloadInput")
        hide("downloadSolved")
        hide("downloadsMsg")
        hide("result")
        file_ok <- TRUE


        inFile <- input$file1


        if (!is.null(inFile)){
            shinyjs::show("processing_gif")
            library(ape)
            library(stringr)
            library(purrr)
            library(phangorn)
            library(pracma)
            library(textclean)
            library(data.table)
            library(tools)
            file_contents <- trimws(readChar(inFile$datapath, file.info(inFile$datapath)$size), which = c("both"))
            if(substr(file_contents,nchar(file_contents),nchar(file_contents)) != ";"){
                hide("processing_gif")
                write(paste(Sys.time(),";","end of file error",sep = ""), "activities.log", append = TRUE)
                shinyalert("Oops!", "Please make \";\" as the last character in the phylogenies file.", type = "error")
            }else{
                shinyjs::show("checking_msg")
                trees <- strsplit(file_contents,";")

                # add space after each comma so that the string is wrapped in the trees table
                for(x in 1:length(trees[[1]])){
                    trees[[1]][x] <- gsub(",([A-Za-z (])", ", \\1", trees[[1]][x])
                }


                res_parentheses <- ""
                res_repeated <- ""
                i <- 1
                for (tree in trees[[1]]) {
                    if(str_count(tree,"\\(") != str_count(tree,"\\)")){
                        file_ok <- FALSE
                        if(res_parentheses == ""){
                            res_parentheses <- paste(res_parentheses, i, sep = "")
                        }else{
                            res_parentheses <- paste(res_parentheses, i, sep = ", ")
                        }
                    }

                    res <- strsplit(str_squish(gsub(",", " ", gsub("\\)"," ",gsub("\\(", " ",replace_white(tree)))))," ")
                    counter <- 0
                    for (tip in res[[1]]) {
                        counter <- 0
                        for (tmp in res[[1]]) {
                            if(tmp == tip){
                                counter <- counter + 1
                            }
                        }

                        if(counter > 1){
                            break
                        }
                    }

                    if(counter > 1){
                        #file_ok <- FALSE
                        if(res_repeated == ""){
                            res_repeated <- paste(res_repeated, i, sep = "")
                        }else{
                            res_repeated <- paste(res_repeated, i, sep = ", ")
                        }
                    }
                    i <- i+1

                }

                hide("checking_msg")
                hide("processing_gif")


                if(file_ok){
                    shinyjs::show("bulid_superareagram")
                    shinyjs::show("note")
                    shinyjs::show("clearRows")
                    trees_global <<- trees[[1]]
                    output$treesTable <- DT::renderDataTable({
                        treesTable.data <- data.frame(
                            Tree = trees[[1]]
                        )
                        DT::datatable(treesTable.data, options = list(searching = FALSE))
                    })


                }else{
                    message <- ""
                    if(res_parentheses != ""){
                        write(paste(Sys.time(),";","unbalanced parentheses",sep = ""), "activities.log", append = TRUE)
                        message <- paste(message, "\nPhylogeny(ies) with unbalanced parentheses No:", res_parentheses, sep = " ")
                    }

                    # if(res_repeated != ""){
                    #     write(paste(Sys.time(),";","repeated taxons",sep = ""), "activities.log", append = TRUE)
                    #     message <- paste(message, "\nPhylogeny(ies) with repeated taxons No:", res_repeated, sep = " ")
                    # }

                    if(message != ""){
                        hide("processing_gif")
                        shinyalert("Oops!", message, type = "error")
                    }
                }

            }
        }

    })

    observeEvent(input$bulid_superareagram, {

      library(lubridate)
      library(raster)
      library(pracma)
      library(stringr)
      library(ape)
      library(purrr)
      library(phangorn)
      library(textclean)
      library(tools)
      library(utils)
      library(readtext)

      hide("downloadDataSS")
      hide("downloadDataNEX")
      hide("downloadInput")
      hide("downloadSolved")
      hide("downloadsMsg")
      hide("result")
      shinyjs::show("processing_gif")
      shinyjs::disable("bulid_superareagram")


      tryCatch({
          s = input$treesTable_rows_selected
          inputTrees <<- ""
          if (length(s)) {
              for(x in s){
                  inputTrees <<- paste0(paste0(inputTrees, trees_global[[x]],";"), "\n")
              }
          }else{
              for(x in trees_global){
                  inputTrees <<- paste0(inputTrees, x,";")
              }
          }


          samba <- function(ag_tree_text, ag_tree_phylo = NULL, current_ag = NULL, current_ag_repetitions = NULL){
            if (is.null(current_ag) || current_ag_repetitions == 1){

              res_duplicated_ag <- vector()
              ag_tree_phylo <- read.tree(text = ag_tree_text)

              for (tip in ag_tree_phylo$tip.label) {
                if (str_detect(tip, "#")){
                  tip_split = strsplit(tip,"#")
                  res_duplicated_ag <- c(res_duplicated_ag, tip_split[[1]])
                }else{
                  res_duplicated_ag <- c(res_duplicated_ag, tip)
                }
              }

              res_duplicated_ag <- trimws(res_duplicated_ag)
              res_duplicated_unique_ag <- unique(res_duplicated_ag[duplicated(res_duplicated_ag)])

              if (length(res_duplicated_unique_ag) > 0){
                res_duplicated_table_ag <- table(unlist(res_duplicated_ag))
                current_ag <- res_duplicated_unique_ag[[1]][[1]]
                return(samba(ag_tree_text, ag_tree_phylo, current_ag, res_duplicated_table_ag[[current_ag]]))

              }else{ # no more paralogy, stop the recursion
                #print(convert_phylo_to_text(ag_tree_phylo))
                if (str_detect(ag_tree_text, "#")){ # MAST
                  i <- 1
                  while(i <= length(ag_tree_phylo$tip.label)){
                    tip <- ag_tree_phylo$tip.label[i]
                    if (str_detect(tip, "#")){
                      res <- which(ag_tree_phylo$edge[,2] == i)
                      parent_node <- ag_tree_phylo$edge[res,1]
                      tip_components <- strsplit(tip,"#")
                      tip_components <- tip_components[[1]]
                      ag_tree_phylo$tip.label[i] <- tip_components[1]
                      tip_components <- tip_components[tip_components != tip_components[1]]
                      ag_tree_phylo <- add.tips(ag_tree_phylo, tip_components, parent_node)
                      i <- 1
                    }
                    i <- i + 1
                  }
                }

                return(convert_phylo_to_text(ag_tree_phylo))
              }

            }else{
              # identify the first occurunce of the current taxa, then create two copies of the phylo tree, one with the first occurence
              # of the current taxa and the other with the rest, then call paralogy again on both trees
              first_occurrence <- TRUE
              first_occurrence_index <- 0
              rest_indexes <- list()
              i <- 1

              tree_first_ag_phylo <- ag_tree_phylo
              tree_rest_ag_phylo <- ag_tree_phylo

              for (tip in ag_tree_phylo$tip.label) {
                if (str_detect(tip, "#")){
                  tip_components <- strsplit(tip,"#")
                  if (current_ag %in% tip_components[[1]]){
                    if (first_occurrence == TRUE){
                      tree_rest_ag_phylo$tip.label[i] <- paste(tip_components[[1]][tip_components[[1]] != current_ag],  collapse = '#')
                      first_occurrence <- FALSE
                    }else{
                      tree_first_ag_phylo$tip.label[i] <- paste(tip_components[[1]][tip_components[[1]] != current_ag],  collapse = '#')
                    }
                  }
                }else{
                  if (current_ag == tip){
                    if (first_occurrence == TRUE){
                      first_occurrence_index <- i
                      first_occurrence <- FALSE
                    }else{
                      rest_indexes <- c(rest_indexes, i)
                    }
                  }
                }

                i <- i + 1
              }

              if (length(rest_indexes) > 0){
                tree_first_ag_phylo <- drop.tip(tree_first_ag_phylo, rest_indexes[[1]])
              }

              if (first_occurrence_index != 0){
                tree_rest_ag_phylo <- drop.tip(tree_rest_ag_phylo, first_occurrence_index)
              }

              tree_1 <- samba(convert_phylo_to_text(tree_first_ag_phylo), tree_first_ag_phylo, current_ag, 1)
              tree_2 <- samba(convert_phylo_to_text(tree_rest_ag_phylo), tree_rest_ag_phylo, current_ag, current_ag_repetitions - 1)
              return(paste0(tree_1, tree_2))
            }
          }

          ################
          convert_phylo_to_text <- function(tree){
            tree<-reorder.phylo(tree,"cladewise")
            n<-length(tree$tip)
            string<-vector(); string[1]<-"("; j<-2
            for(i in 1:nrow(tree$edge)){
              if(tree$edge[i,2]<=n){
                string[j]<-tree$tip.label[tree$edge[i,2]]; j<-j+1
                if(!is.null(tree$edge.length)){
                  string[j]<-paste(c(":",round(tree$edge.length[i],10)), collapse="")
                  j<-j+1
                }
                v<-which(tree$edge[,1]==tree$edge[i,1]); k<-i
                while(length(v)>0&&k==v[length(v)]){
                  string[j]<-")"; j<-j+1
                  w<-which(tree$edge[,2]==tree$edge[k,1])
                  if(!is.null(tree$edge.length)){
                    string[j]<-paste(c(":",round(tree$edge.length[w],10)), collapse="")
                    j<-j+1
                  }
                  v<-which(tree$edge[,1]==tree$edge[w,1]); k<-w
                }
                string[j]<-","; j<-j+1
              } else if(tree$edge[i,2]>=n){
                string[j]<-"("; j<-j+1
              }
            }


            if (is.null(tree$edge.length)){
              string<-c(string[1:(length(string)-1)], ";")
            } else {
              string<-c(string[1:(length(string)-2)],";")
            }

            string<-paste(string, collapse="")
            return (string)
          }

          ##########################################################################


          while(dir.exists(paste0(server_path, "www/", ID <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), sample(100000:999999, 1))))){}

          dir.create(upload_dir <- paste0(server_path, "www/", ID))

          cat(inputTrees, file = paste0(upload_dir, "/input_trees.tre"))

          input_trees_file_path <- paste0(server_path, "www/", ID, "/input_trees.tre")
          file_contents <- readtext(input_trees_file_path)
          all_trees <- strsplit(file_contents$text,";")
          samba_res <- ""

          for (tree in all_trees[[1]]) {
            tmp_samba_res <- samba(gsub("[\r\n]", "", paste0(tree, ";")))
            samba_res <- paste0(samba_res, tmp_samba_res)
          }

          total_number <- sum(unlist(strsplit(samba_res,"")) == ";")
          cat(gsub("[;]", ";\n", samba_res), file = paste0(server_path, "www/", ID, "/samba_res.tre"))

          nw_order_path <- if (dir.exists("/Users/muhsen/Downloads/newick-utils-1.6/src/"))
            "/Users/muhsen/Downloads/newick-utils-1.6/src/nw_order" else "/srv/shiny-server/newick-utils/src/nw_order"

          ordered_samba_res <- system(paste0(nw_order_path," \"",server_path, "www/", ID, "/samba_res.tre\""), intern = TRUE)
          ordered_samba_res_unique <<- unique(ordered_samba_res)
          ordered_samba_res_unique <<- paste(unique(ordered_samba_res),  collapse = '\n')
          total_number_unique <- sum(unlist(strsplit(ordered_samba_res_unique,"")) == ";")

          cat(ordered_samba_res_unique, file = paste0(server_path, "www/", ID,  "/samba_res_unique.tre"))

          #################### BuM
          supermatrix_res_ss <<- ""
          supermatrix_res_nex <<- ""
          my.trees <- read.tree(paste0(server_path, "www/", ID,  "/samba_res_unique.tre"))

          if(inherits(my.trees,"phylo")) {
            my.trees<-list(my.trees)
            class(my.trees)<-"multiPhylo"
          }

          N <- length(my.trees)
          my.uniqueTipLabels <- unique(unlist(my.trees%>%map("tip.label")))
          max_tip_length <- if(max(nchar(my.uniqueTipLabels)) >= nchar("Outgroup")) max(nchar(my.uniqueTipLabels)) else nchar("Outgroup")
          my.supermatrices <- list()
          totalColLength <- 0

          for (tree in my.trees) {
            totalColLength <- totalColLength + (tree$Nnode - 1)
            internal_nodes <- (length(tree$tip.label)+2):(length(tree$tip.label)+tree$Nnode)
            i <- 1
            supermatrix = matrix(0,nrow = tree$Nnode - 1, ncol = length(tree$tip.label))

            for (tip in tree$tip.label) {
              x <- i
              vec <- vector()
              while (length(res <- which(tree$edge[,2] == x)) != 0) {
                vec <- c(vec, x <- tree$edge[res,1])
              }

              tipRow <- vector()
              for (internal_node in internal_nodes) {
                if(internal_node %in% vec){
                  tipRow <- c(tipRow,1)
                }else{
                  tipRow <- c(tipRow,0)
                }
              }
              supermatrix[,i] <- tipRow
              i <- i+1
            }

            my.supermatrices[[length(my.supermatrices)+1]] <- t(supermatrix)

          }

          my.supermatrix = matrix(0, nrow = length(my.uniqueTipLabels), ncol = totalColLength, dimnames=list(my.uniqueTipLabels))
          x <- 1

          for (tipLabel in my.uniqueTipLabels) {
            i <- 1
            vec <- vector()
            for (tree in my.trees) {
              if(length(tipIndex <- which(tree$tip.label == tipLabel)) != 0){
                vec <- c(vec,my.supermatrices[[i]][tipIndex,])
              }else{
                vec <- c(vec, rep('?',ncol(my.supermatrices[[i]])))
              }
              i <- i + 1
            }
            my.supermatrix[x,] <- vec
            x <- x+1


          }

          supermatrix_res_ss <<- paste0("xread\n\'\'",ncol(my.supermatrix)," ",(nrow(my.supermatrix)+1),"\n")
          supermatrix_res_nex <<- paste0("#NEXUS\n\nBEGIN DATA;\n\nDIMENSIONS  NTAX=",(nrow(my.supermatrix)+1), " NCHAR=",
                                       ncol(my.supermatrix),";","\n\nFORMAT MISSING=? GAP=-;\n\nMATRIX\n\n")

          str <- paste0(str_pad("Outgroup", max_tip_length,"right"), "  ")
          for (i in 1:ncol(my.supermatrix)) {
            str <- paste0(str,"0")
          }

          supermatrix_res_ss <<- paste0(supermatrix_res_ss,str,"\n")
          supermatrix_res_nex <<- paste0(supermatrix_res_nex,str,"\n")

          for (row in 1:nrow(my.supermatrix)) {
            supermatrix_res_ss <<- paste0(supermatrix_res_ss,str_pad(my.uniqueTipLabels[row], max_tip_length,"right"), "  ", paste(my.supermatrix[row,],collapse=""),"\n")
            supermatrix_res_nex <<- paste0(supermatrix_res_nex,str_pad(my.uniqueTipLabels[row], max_tip_length,"right"), "  ", paste(my.supermatrix[row,],collapse=""),"\n")

          }

          supermatrix_res_ss <<- paste0(supermatrix_res_ss,";")
          supermatrix_res_nex <<- paste0(supermatrix_res_nex,";\nEND;")

          cat(supermatrix_res_ss, file = paste0(server_path, "www/", ID,  "/samba_mrp_matrix.ss"))
          cat(supermatrix_res_nex, file = paste0(server_path, "www/", ID,  "/samba_mrp_matrix.nex"))

          shinyjs::show("downloadDataSS")
          shinyjs::show("downloadDataNEX")
          shinyjs::show("downloadInput")
          shinyjs::show("downloadSolved")
          shinyjs::show("downloadsMsg")
          shinyjs::show("result")
          shinyjs::enable("bulid_superareagram")
          hide("processing_gif")
          unlink(paste0(server_path, "www/", ID), recursive = TRUE, force = TRUE)


      }, error = function(e)
      {
          print(e)
      }) # try catch end

    })

    output$downloadDataSS <- downloadHandler(
      filename = function() {
        paste0(file_path_sans_ext(basename(input$file1$name)),"_SAMBA.ss")
      },
      content = function(file) {
        write(supermatrix_res_ss,file)
        write(paste0(activities_log,";ss"), "activities.log", append = TRUE)
      }
    )

    output$downloadDataNEX <- downloadHandler(
      filename = function() {
        paste0(file_path_sans_ext(basename(input$file1$name)),"_SAMBA.nex")
      },
      content = function(file) {
        write(supermatrix_res_nex,file)
        write(paste0(activities_log,";nex"), "activities.log", append = TRUE)
      }
    )

    output$downloadInput <- downloadHandler(
      filename = function() {
        paste0(file_path_sans_ext(basename(input$file1$name)),"_input_trees_SAMBA.tre")
      },
      content = function(file) {
        write(inputTrees,file)
        write(paste0(activities_log,";nex"), "activities.log", append = TRUE)
      }
    )

    output$downloadSolved <- downloadHandler(
      filename = function() {
        paste0(file_path_sans_ext(basename(input$file1$name)),"_solved_trees_SAMBA.tre")
      },
      content = function(file) {
        write(ordered_samba_res_unique,file)
        write(paste0(activities_log,";nex"), "activities.log", append = TRUE)
      }
    )

    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {
        modalDialog(
            h4("The parsimony analysis will be based on the following parameters:"),
            span(HTML("<strong>Method:</strong> Fitch<br /><strong>Rearrangements:</strong> SPR<br /><strong>Perturbation:</strong> Ratchet<br /><br />")),
            numericInput("maxit", "Maximum iterations number:", 1000, min = 1, max = 10000),
            br(),
            numericInput("k", "k:", 3, min = 1, max = 10),
            br(),
            textInput("userEmail", "Email (to receive the results):"),
            #if (failed)
            #div(tags$b("Invalid name of data object", style = "color: red;")),

            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
            )
        )
    }
}

# Run the application
shinyApp(ui = ui, server = server)
