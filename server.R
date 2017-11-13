
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)



shinyServer(function(input, output) {

  taxes = function(stipend, tuition, status, exempt){
    if(status == "single"){
      standard_deduction = 6350
      if(stipend <= 261500){
        exemption = as.numeric(exempt) * 4050
      }
      if(stipend > 261500){
        exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 261500)/2500)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      if(taxable_income <= 9325){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 9326 & taxable_income <= 37950){current_tax = 932.5 + .15*(taxable_income - 9325)}
      if(taxable_income >= 37951 & taxable_income <= 91900){current_tax = 5226.25 + .25*(taxable_income - 37950)}
      if(taxable_income >= 91901 & taxable_income <= 191650){current_tax = 18713.75 + .28*(taxable_income - 91900)}
      if(taxable_income >= 191651 & taxable_income <= 416700){current_tax = 46643.75 + .33*(taxable_income - 191650)}
      if(taxable_income >= 416701 & taxable_income <= 418400){current_tax = 120910.25 + .35*(taxable_income - 416700)}
      if(taxable_income >= 418401){current_tax = 121505.25 + .396*(taxable_income - 418400)}
      
      if(stipend + tuition <= 261500){
        proposed_exemption = as.numeric(exempt) * 4050
      }
      if(stipend + tuition > 261500){
        proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 261500)/2500)/100))
      }
      
      proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
      
      if(proposed_taxable_income <= 9325){proposed_tax = 0.1 * proposed_taxable_income}
      if(proposed_taxable_income >= 9326 & proposed_taxable_income <= 37950){proposed_tax = 932.5 + .15*(proposed_taxable_income - 9325)}
      if(proposed_taxable_income >= 37951 & proposed_taxable_income <= 91900){proposed_tax = 5226.25 + .25*(proposed_taxable_income - 37950)}
      if(proposed_taxable_income >= 91901 & proposed_taxable_income <= 191650){proposed_tax = 18713.75 + .28*(proposed_taxable_income - 91900)}
      if(proposed_taxable_income >= 191651 & proposed_taxable_income <= 416700){proposed_tax = 46643.75 + .33*(proposed_taxable_income - 191650)}
      if(proposed_taxable_income >= 416701 & proposed_taxable_income <= 418400){proposed_tax = 120910.25 + .35*(proposed_taxable_income - 416700)}
      if(proposed_taxable_income >= 418401){proposed_tax = 121505.25 + .396*(proposed_taxable_income - 418400)}
    }
    if(status == "married joint" | status == "surviving spouse"){
      standard_deduction = 12700
      if(stipend <= 313800){
        exemption = as.numeric(exempt) * 4050
      }
      if(stipend > 313800){
        exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 313800)/2500)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      if(taxable_income <= 18650){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 18651 & taxable_income <= 75900){current_tax = 1865 + .15*(taxable_income - 18650)}
      if(taxable_income >= 75901 & taxable_income <= 153100){current_tax = 10452.5 + .25*(taxable_income - 75900)}
      if(taxable_income >= 153101 & taxable_income <= 233350){current_tax = 29752.5 + .28*(taxable_income - 153100)}
      if(taxable_income >= 233351 & taxable_income <= 416700){current_tax = 52222.5 + .33*(taxable_income - 233350)}
      if(taxable_income >= 416701 & taxable_income <= 470700){current_tax = 112728 + .35*(taxable_income - 416700)}
      if(taxable_income >= 470701){current_tax = 131628 + .396*(taxable_income - 470700)}
      
      if(stipend + tuition <= 313800){
        proposed_exemption = as.numeric(exempt) * 4050
      }
      if(stipend + tuition > 313800){
        proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 313800)/2500)/100))
      }
      
      proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
      if(proposed_taxable_income <= 18650){proposed_tax = 0.1 * proposed_taxable_income}
      if(proposed_taxable_income >= 18651 & proposed_taxable_income <= 75900){proposed_tax = 1865 + .15*(proposed_taxable_income - 18650)}
      if(proposed_taxable_income >= 75901 & proposed_taxable_income <= 153100){proposed_tax = 10452.5 + .25*(proposed_taxable_income - 75900)}
      if(proposed_taxable_income >= 153101 & proposed_taxable_income <= 233350){proposed_tax = 29752.5 + .28*(proposed_taxable_income - 153100)}
      if(proposed_taxable_income >= 233351 & proposed_taxable_income <= 416700){proposed_tax = 52222.5 + .33*(proposed_taxable_income - 233350)}
      if(proposed_taxable_income >= 416701 & proposed_taxable_income <= 470700){proposed_tax = 112728 + .35*(proposed_taxable_income - 416700)}
      if(proposed_taxable_income >= 470701){proposed_tax = 131628 + .396*(proposed_taxable_income - 470700)}
    }
    if(status == "married separate"){
      standard_deduction = 6350
      
      if(stipend <= 156900){
        exemption = as.numeric(exempt) * 4050
      }
      if(stipend > 156900){
        exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 313800)/1250)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      if(taxable_income <= 9325){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 9326 & taxable_income <= 37950){current_tax = 932.5 + .15*(taxable_income - 9325)}
      if(taxable_income >= 37951 & taxable_income <= 76550){current_tax = 5226.25 + .25*(taxable_income - 37950)}
      if(taxable_income >= 76551 & taxable_income <= 116675){current_tax = 14876.25 + .28*(taxable_income - 76550)}
      if(taxable_income >= 116676 & taxable_income <= 208350){current_tax = 26111.25 + .33*(taxable_income - 116675)}
      if(taxable_income >= 208351 & taxable_income <= 235350){current_tax = 56364 + .35*(taxable_income - 208350)}
      if(taxable_income >= 235351){current_tax = 65814 + .396*(taxable_income - 235350)}
      
      if(stipend + tuition <= 156900){
        proposed_exemption = as.numeric(exempt) * 4050
      }
      if(stipend + tuition > 156900){
        proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 313800)/1250)/100))
      }
      
      proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
      
      if(proposed_taxable_income <= 9325){proposed_tax = 0.1 * proposed_taxable_income}
      if(proposed_taxable_income >= 9326 & proposed_taxable_income <= 37950){proposed_tax = 932.5 + .15*(proposed_taxable_income - 9325)}
      if(proposed_taxable_income >= 37951 & proposed_taxable_income <= 76550){proposed_tax = 5226.25 + .25*(proposed_taxable_income - 37950)}
      if(proposed_taxable_income >= 76551 & proposed_taxable_income <= 116675){proposed_tax = 14876.25 + .28*(proposed_taxable_income - 76550)}
      if(proposed_taxable_income >= 116676 & proposed_taxable_income <= 208350){proposed_tax = 26111.25 + .33*(proposed_taxable_income - 116675)}
      if(proposed_taxable_income >= 208351 & proposed_taxable_income <= 235350){proposed_tax = 56364 + .35*(proposed_taxable_income - 208350)}
      if(proposed_taxable_income >= 235351){proposed_tax = 65814 + .396*(proposed_taxable_income - 235350)}
    }
    if(status == "head of household"){
      standard_deduction = 9350
      
      if(stipend <= 287650){
        exemption = as.numeric(exempt) * 4050
      }
      if(stipend > 287650){
        exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend - 287650)/2500)/100))
      }
      taxable_income = stipend - standard_deduction - exemption
      
      if(taxable_income <= 13350){current_tax = 0.1 * taxable_income}
      if(taxable_income >= 13351 & taxable_income <= 50800){current_tax = 1335 + .15*(taxable_income - 13350)}
      if(taxable_income >= 50801 & taxable_income <= 131200){current_tax = 6952.5 + .25*(taxable_income - 50800)}
      if(taxable_income >= 131201 & taxable_income <= 212500){current_tax = 27052.50 + .28*(taxable_income - 131200)}
      if(taxable_income >= 212501 & taxable_income <= 416700){current_tax = 49816.50 + .33*(taxable_income - 212500)}
      if(taxable_income >= 416701 & taxable_income <= 444550){current_tax = 117202.5 + .35*(taxable_income - 416700)}
      if(taxable_income >= 444551){current_tax = 126950 + .396*(taxable_income - 444550)}
      
      if(stipend + tuition <= 287650){
        proposed_exemption = as.numeric(exempt) * 4050
      }
      if(stipend + tuition > 287650){
        proposed_exemption = (as.numeric(exempt)*4050)*(1 - (2*ceiling((stipend + tuition - 287650)/2500)/100))
      }
      
      proposed_taxable_income = stipend + tuition - standard_deduction - proposed_exemption
      
      if(proposed_taxable_income <= 13350){proposed_tax = 0.1 * proposed_taxable_income}
      if(proposed_taxable_income >= 13351 & proposed_taxable_income <= 50800){proposed_tax = 1335 + .15*(proposed_taxable_income - 13350)}
      if(proposed_taxable_income >= 50801 & proposed_taxable_income <= 131200){proposed_tax = 6952.5 + .25*(proposed_taxable_income - 50800)}
      if(proposed_taxable_income >= 131201 & proposed_taxable_income <= 212500){proposed_tax = 27052.50 + .28*(proposed_taxable_income - 131200)}
      if(proposed_taxable_income >= 212501 & proposed_taxable_income <= 416700){proposed_tax = 49816.50 + .33*(proposed_taxable_income - 212500)}
      if(proposed_taxable_income >= 416701 & proposed_taxable_income <= 444550){proposed_tax = 117202.5 + .35*(proposed_taxable_income - 416700)}
      if(proposed_taxable_income >= 444551){proposed_tax = 126950 + .396*(proposed_taxable_income - 444550)}
    }
    
    return(list(`Current Estimated Federal Tax` = current_tax, 
                `% of Stipend paid in tax`=paste0(round(current_tax/stipend * 100,2),"%"),
                `Proposed Estimated Federal Tax` = proposed_tax,
                `% of Stipend paid in tax` = paste0(round(proposed_tax/stipend * 100,2),"%")))
  }
  
  output$current_tax <- renderText({
    taxes(input$stipend,input$tuition,input$status,input$exempt)[[1]]
    })
  output$current_tax_percent <- renderText({
    taxes(input$stipend,input$tuition,input$status,input$exempt)[[2]]
  })
  output$proposed_tax <- renderText({
    taxes(input$stipend,input$tuition,input$status,input$exempt)[[3]]
  })
  output$proposed_tax_percent <- renderText({
    taxes(input$stipend,input$tuition,input$status,input$exempt)[[4]]
  })

})


#taxes(25000,50000,"single",1)[[1]]
