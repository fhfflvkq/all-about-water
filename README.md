## Computation PET
ChungJu$PET <- thornthwaite(ChungJu$TMED, 37.0041)        ##SPEI 사용, 충주댐 위도 입력


GR4HChungJu <- read_excel("GR4HChungJu.xlsx")     ##GR4H 필요한 자료 입력

summary(GR4HChungJu)
data("Param_Sets_GR4J")
str(Param_Sets_GR4J)
summary(Param_Sets_GR4J)

Param_Sets_GR4J$X4 <- Param_Sets_GR4J$X4u / 5.995 * 6648^0.3   ##충주댐 저수지 면적 입력 (6648)
Param_Sets_GR4J$X4u <- NULL
Param_Sets_GR4J <- as.matrix(Param_Sets_GR4J)

#preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4H, DatesR = GR4HChungJu$DatesR,
                                 Precip = GR4HChungJu$P, PotEvap = GR4HChungJu$E)

## --- calibration step

##short calibration period selection (< 6 month)  ##보정기간 6달 미만일 때 방식 
Ind_Cal <- seq(which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-06-25 00:00"),
               which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-07-24 23:00"))

##preparation of the Runoptions object for the calibration period
RunOptions_Cal <- CreateRunOptions(FUN_MOD = RunModel_GR4H,
                                   InputsModel = InputsModel, IndPeriod_Run = Ind_Cal)

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit_Cal  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                    RunOptions = RunOptions_Cal, Obs = GR4HChungJu$Qmm[Ind_Cal])


## ---- validation step

## validation period selection
Ind_Val <- seq(which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-01-01 00:00"), 
               which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-12-31 23:00"))

## preparation of the RunOptions object for the validation period
RunOptions_Val <- CreateRunOptions(FUN_MOD = RunModel_GR4H,
                                   InputsModel = InputsModel, IndPeriod_Run = Ind_Val)

## efficiency criterion (Nash-Sutcliffe Efficiency) on the validation period
InputsCrit_Val  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                    RunOptions = RunOptions_Val, Obs = GR4HChungJu$Qmm[Ind_Val])


OutputsCrit_Loop <- apply(Param_Sets_GR4J, 1, function(iParam) {
  OutputsModel_Cal <- RunModel_GR4H(InputsModel = InputsModel, RunOptions = RunOptions_Cal,
                                    Param = iParam)
  OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit_Cal, OutputsModel = OutputsModel_Cal)
  return(OutputsCrit$CritValue)
})


Param_Best <- unlist(Param_Sets_GR4J[which.max(OutputsCrit_Loop), ])
Param_Best


##load of catchment data
GR4HChungJu

##preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4H, DatesR = GR4HChungJu$DatesR,
                                 Precip = GR4HChungJu$P, PotEvap = GR4HChungJu$E)

## run period selection
Ind_Run <- seq(which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-06-25 00:00"),
               which(format(GR4HChungJu$DatesR, format = "%Y-%m-%d %H:%M")=="2020-07-24 23:00"))


## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4H,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 144.300000, X2 = 0.800000, X3 = 41.000000, X4 = 6.057841 )
OutputsModel <- RunModel_GR4H(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)


## results preview
plot(OutputsModel, Qobs = GR4HChungJu$Qmm[Ind_Run])


## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = GR4HChungJu$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

