figure-timeseries/index.html: figure-timeseries.R velos.RData bike.paths.RData counter.locations.RData places.RData
	R --no-save < $<
figure-bike-paths.png: figure-bike-paths.R counter.locations.RData bike.paths.RData
	R --no-save < $<
counter.locations.RData: counter.locations.R
	R --no-save < $<
bike.paths.RData: bike.paths.R
	R --no-save < $<
figure-places/index.html: figure-places.R places.RData
	R --no-save < $<
places.RData: places.R accidents.RData manual.geocodes.csv
	R --no-save < $<
accidents.RData: accidents.R
	R --no-save < $<
velos.RData: velos.R
	R --no-save < $<
veloscomptage.zip:
	wget http://donnees.ville.montreal.qc.ca/dataset/75d2b58c-73b4-40ed-9f54-6cb1e2e4b225/resource/26a7c453-fda1-4730-854e-bef0d1cb199e/download/veloscomptage.zip
