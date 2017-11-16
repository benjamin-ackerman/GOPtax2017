
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  # First defined a function to calculate taxes
  taxes = function(stipend, tuition, status, exempt){
    # Calculations for single filers
    if(status == "single"){
      exempt = 1
      standard_deduction = 6500
      if(stipend <= 266700){
        exemption = as.numeric(exempt) * 4150
      }
      if(stipend > 266700){
        exemption = (as.numeric(exempt)*4150)*(1 - (2*ceiling((stipend - 266700)/2500)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      # 2018 tax estimate
      if(taxable_income <= 9525){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 9526 & taxable_income <= 38700){current_tax = .1*9525 + .15*(taxable_income - 9525)}
      if(taxable_income >= 38701 & taxable_income <= 93700){current_tax = .1*9525 + .15*(38700-9525) + .25*(taxable_income - 38700)}
      if(taxable_income >= 93701 & taxable_income <= 195450){current_tax = .1*9525 + .15*(38700-9525) + .25*(93700-38700) + .28*(taxable_income - 93700)}
      if(taxable_income >= 195451 & taxable_income <= 424950){current_tax = .1*9525 + .15*(38700-9525) + .25*(93700-38700) + .28*(195450-93700) + .33*(taxable_income - 195450)}
      if(taxable_income >= 424951 & taxable_income <= 426700){current_tax = .1*9525 + .15*(38700-9525) + .25*(93700-38700) + .28*(195450-93700) + .33*(424950-195450)+ .35*(taxable_income - 424950)}
      if(taxable_income >= 426701){current_tax = .1*9525 + .15*(38700-9525) + .25*(93700-38700) + .28*(195450-93700) + .33*(424950-195450) + .35*(426700-424950) + .396*(taxable_income - 426700)}
      
      # H.R. 1 changes
      proposed_standard_deduction = 12200
      proposed_exemption = 0
      proposed_taxable_income = stipend + tuition - proposed_standard_deduction - proposed_exemption
      
      # 2018 H.R. 1 tax estimate
      if(proposed_taxable_income <= 45000){proposed_tax = 0.12 * proposed_taxable_income}
      if(proposed_taxable_income >= 45001 & proposed_taxable_income <= 200000){proposed_tax = .12*45000 + .25*(proposed_taxable_income - 45000)}
      if(proposed_taxable_income >= 200001 & proposed_taxable_income <= 500000){proposed_tax = .12*45000 + .25*(200000-45000) + .35*(proposed_taxable_income - 200001)}
      if(proposed_taxable_income >= 500001){proposed_tax = .12*45000 + .25*(200000-45000) + .35*(500000 - 200000 - 45000) + .396*(proposed_taxable_income - 500001)}
    }
    # Calculations for married joint filers
    if(status == "married joint"){ #| status == "surviving spouse"){
      standard_deduction = 13000
      exempt = 2
      if(stipend <= 320000){
        exemption = as.numeric(exempt) * 4150
      }
      if(stipend > 320000){
        exemption = (as.numeric(exempt)*4150)*(1 - (2*ceiling((stipend - 320000)/2500)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      # 2018 tax estimate
      if(taxable_income <= 18650){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 18651 & taxable_income <= 75900){current_tax = 1865 + .15*(taxable_income - 18650)}
      if(taxable_income >= 75901 & taxable_income <= 153100){current_tax = 10452.5 + .25*(taxable_income - 75900)}
      if(taxable_income >= 153101 & taxable_income <= 233350){current_tax = 29752.5 + .28*(taxable_income - 153100)}
      if(taxable_income >= 233351 & taxable_income <= 416700){current_tax = 52222.5 + .33*(taxable_income - 233350)}
      if(taxable_income >= 416701 & taxable_income <= 470700){current_tax = 112728 + .35*(taxable_income - 416700)}
      if(taxable_income >= 470701){current_tax = 131628 + .396*(taxable_income - 470700)}
      
      # H.R. 1 changes 
      proposed_standard_deduction = 24400
      proposed_exemption = 0
      proposed_taxable_income = stipend + tuition - proposed_standard_deduction - proposed_exemption
      
      # 2018 H.R. 1 tax estimate
      if(proposed_taxable_income <= 90000){proposed_tax = 0.12 * proposed_taxable_income}
      if(proposed_taxable_income >= 90001 & proposed_taxable_income <= 260000){proposed_tax = .12*90000 + .25*(proposed_taxable_income - 90000)}
      if(proposed_taxable_income >= 260001 & proposed_taxable_income <= 1000000){proposed_tax = .12*90000 + .25*(260000-90000) + .35*(proposed_taxable_income - 260001)}
      if(proposed_taxable_income >= 1000001){proposed_tax = .12*90000 + .25*(260000-90000) + .35*(1000000 - 260000 - 90000) + .396*(proposed_taxable_income - 1000001)}
    }
    
    # Here's code I started working on for other filing categories: However, they're for 2017 estimates and do not have the 2018 House GOP bill brackets. *More work needed to change this*
    
    # if(status == "married separate"){
    #   standard_deduction = 6500
    #   
    #   if(stipend <= 156900){
    #     exemption = as.numeric(exempt) * 4050
    #   }
    #   if(stipend > 156900){
    #     exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 313800)/1250)/100))
    #   }
    #   taxable_income = stipend - standard_deduction - exemption
    #   
    #   if(taxable_income <= 9325){current_tax = 0.1 * taxable_income}
    #   if(taxable_income >= 9326 & taxable_income <= 37950){current_tax = 932.5 + .15*(taxable_income - 9325)}
    #   if(taxable_income >= 37951 & taxable_income <= 76550){current_tax = 5226.25 + .25*(taxable_income - 37950)}
    #   if(taxable_income >= 76551 & taxable_income <= 116675){current_tax = 14876.25 + .28*(taxable_income - 76550)}
    #   if(taxable_income >= 116676 & taxable_income <= 208350){current_tax = 26111.25 + .33*(taxable_income - 116675)}
    #   if(taxable_income >= 208351 & taxable_income <= 235350){current_tax = 56364 + .35*(taxable_income - 208350)}
    #   if(taxable_income >= 235351){current_tax = 65814 + .396*(taxable_income - 235350)}
    #   
    #   if(stipend + tuition <= 156900){
    #     proposed_exemption = as.numeric(exempt) * 4050
    #   }
    #   if(stipend + tuition > 156900){
    #     proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 313800)/1250)/100))
    #   }
    #   
    #   proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
    #   
    #   if(proposed_taxable_income <= 9325){proposed_tax = 0.1 * proposed_taxable_income}
    #   if(proposed_taxable_income >= 9326 & proposed_taxable_income <= 37950){proposed_tax = 932.5 + .15*(proposed_taxable_income - 9325)}
    #   if(proposed_taxable_income >= 37951 & proposed_taxable_income <= 76550){proposed_tax = 5226.25 + .25*(proposed_taxable_income - 37950)}
    #   if(proposed_taxable_income >= 76551 & proposed_taxable_income <= 116675){proposed_tax = 14876.25 + .28*(proposed_taxable_income - 76550)}
    #   if(proposed_taxable_income >= 116676 & proposed_taxable_income <= 208350){proposed_tax = 26111.25 + .33*(proposed_taxable_income - 116675)}
    #   if(proposed_taxable_income >= 208351 & proposed_taxable_income <= 235350){proposed_tax = 56364 + .35*(proposed_taxable_income - 208350)}
    #   if(proposed_taxable_income >= 235351){proposed_tax = 65814 + .396*(proposed_taxable_income - 235350)}
    # }
    # if(status == "head of household"){
    #   standard_deduction = 9550
    #   
    #   if(stipend <= 287650){
    #     exemption = as.numeric(exempt) * 4050
    #   }
    #   if(stipend > 287650){
    #     exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 287650)/2500)/100))
    #   }
    #   taxable_income = stipend - standard_deduction - exemption
    #   
    #   if(taxable_income <= 13350){current_tax = 0.1 * taxable_income}
    #   if(taxable_income >= 13351 & taxable_income <= 50800){current_tax = 1335 + .15*(taxable_income - 13350)}
    #   if(taxable_income >= 50801 & taxable_income <= 131200){current_tax = 6952.5 + .25*(taxable_income - 50800)}
    #   if(taxable_income >= 131201 & taxable_income <= 212500){current_tax = 27052.50 + .28*(taxable_income - 131200)}
    #   if(taxable_income >= 212501 & taxable_income <= 416700){current_tax = 49816.50 + .33*(taxable_income - 212500)}
    #   if(taxable_income >= 416701 & taxable_income <= 444550){current_tax = 117202.5 + .35*(taxable_income - 416700)}
    #   if(taxable_income >= 444551){current_tax = 126950 + .396*(taxable_income - 444550)}
    #   
    #   if(stipend + tuition <= 287650){
    #     proposed_exemption = as.numeric(exempt) * 4050
    #   }
    #   if(stipend + tuition > 287650){
    #     proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 287650)/2500)/100))
    #   }
    #   
    #   proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
    #   
    #   if(proposed_taxable_income <= 13350){proposed_tax = 0.1 * proposed_taxable_income}
    #   if(proposed_taxable_income >= 13351 & proposed_taxable_income <= 50800){proposed_tax = 1335 + .15*(proposed_taxable_income - 13350)}
    #   if(proposed_taxable_income >= 50801 & proposed_taxable_income <= 131200){proposed_tax = 6952.5 + .25*(proposed_taxable_income - 50800)}
    #   if(proposed_taxable_income >= 131201 & proposed_taxable_income <= 212500){proposed_tax = 27052.50 + .28*(proposed_taxable_income - 131200)}
    #   if(proposed_taxable_income >= 212501 & proposed_taxable_income <= 416700){proposed_tax = 49816.50 + .33*(proposed_taxable_income - 212500)}
    #   if(proposed_taxable_income >= 416701 & proposed_taxable_income <= 444550){proposed_tax = 117202.5 + .35*(proposed_taxable_income - 416700)}
    #   if(proposed_taxable_income >= 444551){proposed_tax = 126950 + .396*(proposed_taxable_income - 444550)}
    # }
    
    return(list(`Current Estimated Federal Tax` = current_tax, 
                `% of Stipend paid in tax`=round(current_tax/stipend * 100,2),
                `Proposed Estimated Federal Tax` = proposed_tax,
                `% of Stipend paid in tax` = round(proposed_tax/stipend * 100,2)))
  }

  output$current_tax <- renderText({
    paste0("$",taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]])
    })
  
  output$current_tax_percent <- renderText({
    paste0(taxes(input$stipend,input$tuition,input$status,input$exempt)[[2]],"%")
  })
  
  output$proposed_tax <- renderText({
    paste0("$",taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]])
  })

  output$proposed_tax_percent <- renderText({
    paste0(taxes(input$stipend,input$tuition,input$status,input$exempt)[[4]],"%")
  })

  output$plotdiff <- renderPlot({
    if(taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]] < taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]){
    barplot(c(taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]],
              taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]),
            names=c("No change to tax law","estimate under H.R. 1"),
            main="Estimated 2018 Federal Taxes",
            ylab='Dollars ($)', col=rgb(1,0,0,.5), border = FALSE)
    segments(.6,taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]+50, 
             .6,taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]-50)
    arrows(.6,taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]+50, 
             .6,taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]-50,
             lwd = 1.5, angle = 90,code = 3, length = 0.05)
    text(.8,median(taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]:taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]),
         paste0("$",round(taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]-
                            taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]])))}
    
    if(taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]] < taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]){
      barplot(c(taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]],
                taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]),
              names=c("No change to tax law","estimate under H.R. 1"),
              main="Estimated 2018 Federal Taxes",
              ylab='Dollars ($)', col=rgb(1,0,0,.5), border = FALSE)
      segments(1.8,taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]+20, 
               1.8,taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]-20)
      arrows(1.8,taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]+20, 
             1.8,taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]-0,
             lwd = 1.5, angle = 90,code = 3, length = 0.05)
      text(2,median(taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]:taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]),
           paste0("$",round(taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]-
                              taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]])))}
  })

})
