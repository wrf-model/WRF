  INTERFACE
    SUBROUTINE fm_decoder (fm, platform, synop, ship,  metar, &
                                         pilot, sound, satem, &
                                         satob, airep, gpspw, &
                                         gpszd, gpsrf, gpsep, &
                                         ssmt1, ssmt2, ssmi , &
                                         tovs , other, amdar, &
                                         qscat, profl, buoy, bogus, airs, tamdar )
    IMPLICIT NONE
    INTEGER,              INTENT (in)    :: fm
    CHARACTER (LEN = 40), INTENT (out)   :: platform
    INTEGER,              INTENT (inout), OPTIONAL :: synop, ship,  metar, &
                                                      pilot, sound, satem, &
                                                      satob, airep, gpspw, &
                                                      gpszd, gpsrf, gpsep, &
                                                      bogus, &
                                                      ssmt1, ssmt2, ssmi,  &
                                                      tovs,  amdar, qscat, &
                                                      profl, buoy, airs, tamdar, other
    END SUBROUTINE fm_decoder
  END INTERFACE

