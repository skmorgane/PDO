library(WaveletComp)
library(forecast)

##################### FUNCTIONS 

extract_scale_transform = function(wavelet_object, periods)
{
  headers = character()
  scale_matrix = c()
  for (period in periods)
  {
    closest_period = which.min(abs(wavelet_object$Period - period))
    period_wavelet = Re(wavelet_object$Wave[closest_period,])
    headers = append(headers, paste("period",as.character(period), sep=""))
    scale_matrix = cbind(scale_matrix, period_wavelet)
    colnames(scale_matrix) = headers
  }
  output = data.frame(scale_matrix, wavelet_object$series['date'])
}

##################### MAIN CODE

list_datafiles = list.files(pattern="*021992_122014.csv")
for (i in 1:length(list_datafiles)) assign(list_datafiles[i], read.csv(list_datafiles[i]))



# create wavelet transform data for NDVI, precipitation, and Energy for subsetted data

ppt.w = analyze.wavelet(ppt, "ppt",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(ppt.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Precipitation (1992-2014)")

NDVI.w = analyze.wavelet(NDVI, "NDVI",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(NDVI.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "NDVI (1992-2014)")

energy.w = analyze.wavelet(energy, "energy",  dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(energy.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Rodent Energy (1992-2014)")

PDO.w = analyze.wavelet(PDO, "PDO", dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(PDO.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "PDO Index (1992-2014)")

MEI.w = analyze.wavelet(MEI, "MEI", dt=1, lowerPeriod = 2, upperPeriod = 120, make.pval = T, n.sim = 100)
wt.image(MEI.w, color.key="quantile", n.levels=100, show.date = T, date.format = "%Y-%m-%d",
         legend.params = list(lab = "wavelet power levels", mar = 4.7, label.digits = 2),
         main = "Multivariate ENSO Index (1992-2014)")

# extract specific scales from wavelet transforms

periods = c(6,12,24,48,60)
ppt_scaledata = extract_scale_transform(ppt.w, periods)
NDVI_scaledata = extract_scale_transform(NDVI.w, periods)
energy_scaledata = extract_scale_transform(energy.w, periods)

# spectral density plots

wt.avg(energy.w)
wt.avg(ppt.w)
wt.avg(NDVI.w)
wt.avg(PDO.w)
wt.avg(MEI.w)

# cross-wavelet correlations (no lags to my knowledge)

ppt_ndvi = data.frame(ppt = ppt$ppt, NDVI=NDVI$NDVI, date = ppt$date)
NDVI_ppt = analyze.coherency(ppt_ndvi, my.pair=c("ppt","NDVI"),
                                  loess.span = 0, dt = 1/12,
                                  lowerPeriod = 1/12, make.pval = T,
                                  n.sim=100)
wc.image(NDVI_ppt)
wc.avg(NDVI_ppt)
wc.image(cross_wavelet, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"), main="Coherence between NDVI & ppt")

ndvi_energy = data.frame(NDVI=NDVI$NDVI, rodent=energy$energy, 
                         date = NDVI$date)
NDVI_rodents = analyze.coherency(ndvi_energy,
                                 my.pair=c("NDVI","rodent"),
                                 loess.span = 0, dt = 1/12,
                                 lowerPeriod = 1/12, make.pval = T,
                                 n.sim=100)
wc.image(NDVI_rodents)
wc.avg(NDVI_rodents)
wc.image(NDVI_rodents, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"), main="Coherence between NDVI & Rodents")

ppt_PDO = data.frame(PDO=PDO$PDO, ppt=ppt$ppt, 
                         date = ppt$date)
ppt_PDO = analyze.coherency(ppt_PDO,
                                 my.pair=c("PDO","ppt"),
                                 loess.span = 0, dt = 1/12,
                                 lowerPeriod = 1/12, make.pval = T,
                                 n.sim=100)
wc.image(ppt_PDO)
wc.avg(ppt_PDO)
wc.image(ppt_PDO, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"), main="Coherence between PPT & PDO")


ppt_MEI = data.frame(MEI=MEI$MEI, ppt=ppt$ppt, 
                     date = ppt$date)
ppt_MEI = analyze.coherency(ppt_MEI,
                            my.pair=c("MEI","ppt"),
                            loess.span = 0, dt = 1/12,
                            lowerPeriod = 1/12, make.pval = T,
                            n.sim=100)
wc.image(ppt_MEI)
wc.avg(ppt_MEI)
wc.image(ppt_MEI, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
         list(lab= "wavelet coherence levels"), main="Coherence between PPT & MEI",
         show.date = T, date.format = "%Y-%m-%d",)

rodent_PDO = data.frame(PDO=PDO$PDO, rodent=energy$energy, 
                     date = PDO$date)
rodent_PDO = analyze.coherency(rodent_PDO,
                            my.pair=c("PDO","rodent"),
                            loess.span = 0, dt = 1/12,
                            lowerPeriod = 1/12, make.pval = T,
                            n.sim=100)
wc.image(rodent_PDO)
wc.avg(rodent_PDO)
wc.image(rodent_PDO, which.image = 'wc', n.levels =250,
         siglvl.contour = 0.1, siglvl.arrow = 0.5, legend.params = 
           list(lab= "wavelet coherence levels"), main="Coherence between PDO & rodent",
         show.date = T, date.format = "%Y-%m-%d")

wc.sel.phases(rodent_PDO, sel.period = 2, only.sig = F, siglvl = 0.05,
              which.sig = "wc", show.date = T, date.format = "%Y-%m-%d",
              legend.coords = "topright", legend.horiz = F,
              phaselim = c(-pi,+pi), main = "", sub = "")

