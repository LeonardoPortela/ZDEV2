"Name: \FU:FI_SUBST_GSBER\SE:END\EI
ENHANCEMENT 0 Z_GSBER_AR.
*Inputa campo Divisao.
*break-point.
  "US:75407

data: t_tvarvc  type tvarvc-low.
FIELD-SYMBOLS: <fs_bukrs>.
"75407 LPORTELA
 ASSIGN ('(SAPLFACI)LV_BUKRS') TO <fs_bukrs>.

 IF <fs_bukrs> IS ASSIGNED.

    SELECT SINGLE low
         INTO t_tvarvc
         FROM tvarvc
        WHERE name = 'Z_ZGL_SEM_DIVISAO'
      AND low EQ  <fs_bukrs>.

  ENDIF.
"75407 LPORTELA

IF <fs_bukrs> NE t_tvarvc.

if sy-TCODE EQ 'VF01'
OR SY-TCODE EQ 'VF02'
or sy-tcode eq 'MIRO'
or sy-tcode eq 'FB01'
or sy-tcode eq 'FB05'
or sy-tcode eq 'F110'
or sy-tcode eq 'FB50'.
  data: wl_gsber type accit-gsber,
        WL_BEWAR TYPE ZFIT0030-BEWAR.

  loop at t_accit
     where gsber is not initial.
*       and awtyp eq wl_awtyp.

    wl_gsber = t_accit-gsber.
    exit.
  endloop.

  loop at t_accit
     where gsber is initial.
*       AND AWTYP EQ wl_awtyp.
    t_accit-gsber = wl_gsber.

    modify t_accit.
  endloop.

  ENDIF.

  ENDIF.

ENDENHANCEMENT.
