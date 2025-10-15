*&---------------------------------------------------------------------*
*& Report  ZMMI0003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMI0003.

DATA: VDATAI TYPE SY-DATUM,
      VDATAF TYPE SY-DATUM,
      VG_JOB TYPE I.

VDATAI  = SY-DATUM - 2.
VDATAF  = SY-DATUM.

SELECT SINGLE COUNT(*) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'ATUALIZA_KUHLMANN'
     AND STATUS EQ 'R'.

IF ( VG_JOB EQ 1 ).
  SUBMIT ZMMI0002 WITH P_DATAI = VDATAI
                  WITH P_DATAF = VDATAF
   AND RETURN.
ENDIF.
