figure-places.pdf: figure-places.R places.RData
	R --no-save < $<
places.RData: places.R accidents.RData
	R --no-save < $<
accidents.RData: accidents.R
	R --no-save < $<
timeseries/index.html: figure-timeseries.R velos.RData
	R --no-save < $<
velos.RData: velos.R
	R --no-save < $<
veloscomptage.zip:
	wget http://donnees.ville.montreal.qc.ca/dataset/75d2b58c-73b4-40ed-9f54-6cb1e2e4b225/resource/26a7c453-fda1-4730-854e-bef0d1cb199e/download/veloscomptage.zip
