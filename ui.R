library(ggvis);library(shiny);library(XML);library(leaflet)


ui<-navbarPage("Marine Monitoring Data",
  tabPanel('Marine Offshore',
fluidRow(
    column(8, align="center",
    selectInput('off.site', 'Site:','LSKQ06'),
 #   dateInput('date', 'Sampling Date', value=max(as.Date(data$COLLECTDATE,format='%m/%d/%Y')))
    selectInput('date','Sampling Date',"2016-12-31"),
    checkboxInput('log','Nutrients, TSS on Log-Scale?',F)
   # ,fileInput('database',label='Database',accept=c('mdb','accdb'))
  )),
  tabsetPanel(
tabPanel('Profiles',
# 
 fluidRow(
   column(4,
   ggvisOutput('plot_temp')),
    column(4,
    ggvisOutput('plot_dens'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_do')),
   column(4,
   ggvisOutput('plot_do_lab'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_cond')),
   column(4,
   ggvisOutput('plot_ph'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_sal')),
   column(4,
   ggvisOutput('plot_sal_lab'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_ortho')),
   column(4,
   ggvisOutput('plot_totp'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_nitrate')),
   column(4,
   ggvisOutput('plot_ammonia'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_totn')),
   column(4,
   ggvisOutput('plot_pheo'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_chla')),
   column(4,
   ggvisOutput('plot_chla_field'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_fecal_profile')),
   column(4,
   ggvisOutput('plot_entero_profile'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_coli_profile')),
   column(4,
   ggvisOutput('plot_surface_par'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_par')),
   column(4,
   ggvisOutput('plot_light_trans'))
 ),
 fluidRow(
   column(4,
   ggvisOutput('plot_sil')),
   column(4,
   ggvisOutput('plot_tss'))
 ),
 fluidRow(
   column(8,
   ggvisOutput('plot_secchi'))),
 fluidRow(
   column(8,
   ggvisOutput('plot_int_chla'))
   )
),
tabPanel('Depth over Time',
fluidRow(
    column(8, align="center",
              sliderInput("depth", "Depth Range:",
                  min = 0, max = 200,step=1,
                  value = c(0,2))#,
    # numericInput('minDepth', 'Minimum Depth',0,min=0,max=500,step=1),
    # numericInput('maxDepth', 'Maximum Depth',2,min=0,max=500,step=1)
    )),
fluidRow(
  column(8,
   ggvisOutput('plot_temp_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_salinity_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_salinity_lab_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_do_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_do_lab_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_fecal_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_coli_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_entero_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_ammonia_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_nitrate_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_ortho_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_sil_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_tss_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_pheo_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_chla_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_chla_field_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_light_depth'))),
fluidRow(
  column(8,
   ggvisOutput('plot_par_depth')))
)
)),
tabPanel('Marine Intertidal',
fluidRow(
    column(8, align="center",
    selectInput('it.site', 'Site:','LSHV01'),
    selectInput('it.date','Sampling Date',"2016-12-31"),
    checkboxInput('it.log','Nutrients, TSS on Log-Scale?',F)
  )),
fluidRow(
  column(8,
   ggvisOutput('plot_temp_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_salinity_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_fecal_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_entero_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_coli_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_ammonia_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_nitrate_it'))),
fluidRow(
  column(8,
   ggvisOutput('plot_ortho_it'))
  )  
),
tabPanel('Map of All Sites',
             column(8,align='center',
              dateRangeInput('date_map','Select Date Range for Map',start='2016-12-01',end='2016-12-31'),
              selectInput("parm_comp", "Compare...",
                  choices=c('Temperature'=22,'Dissolved Oxygen, Field'=6,
                            'Salinity, Field'=19,'TSS'=28,
                            'Enterococcus'=8,'Fecal Coliform'=9,
                            'Ammonia'=28,'Nitrate+Nitrite'=14,
                            'Orthophosphate'=15,'Silica'=21,
                            'Cholorophyll a'=1
                            ))),
         sliderInput("depth_map", "Depth Range:",
                  min = 0, max = 200,step=1,
                  value = c(0,2)),
             # numericInput('minDepth_map', 'Minimum Depth',0,min=0,max=500,step=1),
             # numericInput('maxDepth_map', 'Maximum Depth',2,min=0,max=500,step=1),
         textOutput('MAPDATA'),     
         leafletOutput("comparison_map",width='80%',height=800)
             )
)

