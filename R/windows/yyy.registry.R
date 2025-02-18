'.httpDefaultApp' <- function() {
   prog <- try(utils::readRegistry("SoftwarE\\Microsoft\\Windows\\Shell\\Associations\\UrlAssociations\\http\\UserChoice"
                              ,hive="HCU"))#["ProgId"][[1]])
   if (inherits(prog,"try-error"))
      return(NULL)
   prog <- prog[[.grep("ProgID",names(prog))]]
   br <- try(utils::readRegistry(paste0(prog,"\\shell\\open\\command")
                            ,hive="HCR")[["(Default)"]])
   if (inherits(br,"try-error"))
      return(NULL)
   br
}
