#
FROM davegill/wrf-coop:fifthtry
MAINTAINER Dave Gill <gill@ucar.edu>

#RUN echo _HERE1_
#RUN git clone https://github.com/davegill/WRF.git davegill/WRF \
#  && cd davegill/WRF \
#  && git fetch origin +refs/pull/4/merge: \
#  && git checkout -qf FETCH_HEAD \
#  && cd .. \
#  && mv WRF /wrf/WRF
#RUN echo _HERE2_

RUN git clone _FORK_/_REPO_.git WRF \
  && cd WRF \
  && git checkout _BRANCH_ \
  && cd ..

RUN curl -SL http://www2.mmm.ucar.edu/wrf/dave/script.tar | tar -xC /wrf

VOLUME /wrf
CMD ["/bin/tcsh"]
