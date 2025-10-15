*****modificación****
clear v_total_f.
clear v_total_f2.
CLEAR: w_IMP_TOT.
****modificación*****
LOOP AT T_RECIBO_POS INTO ST_RECIBO_POS.
*modificado
*  v_total_f = v_total_f + ST_RECIBO_POS-dmbtr.
*modificado
*modificado 19/03/12
* v_total_f2 = v_total_f2 + ST_RECIBO_POS-dmbtr.
*modificado 19/03/12
*****modificación****19/03/12
"IMPORTE TOTAL
w_IMP_TOT-IMPORTE = w_IMP_TOT-IMPORTE + ST_RECIBO_POS-dmbtr.
at END OF AUGBL.
w_IMP_TOT-AUGBL  = ST_RECIBO_POS-AUGBL.
APPEND  w_IMP_TOT to T_IMP_TOT.
CLEAR: w_IMP_TOT-IMPORTE , w_IMP_TOT-IMPORTE.
ENDAT.
****modificación*****19/03/12
endloop.
*modificado
*WRITE v_total_F to v_total_f2.
*
*clear v_total_f.
*
*CONDENSE v_total_f2 NO-GAPS.
*modificado
WRITE v_total_F2 to v_total_f.

*data: w_bseg_subt like line of t_bseg_subt,
*      w_bseg_tot like line of t_bseg_tot.
*
*DATA: l_moneda TYPE char3.
*
*data: v_dmbtr_v type bseg-dmbtr.
*data: v_importe_v type bseg-dmbtr.
**if ST_RECIBO_POS-waers eq 'ARS'.
** l_moneda = '$'.
**endif.
*data: BEGIN OF t_set OCCURS 0,
*        setname     LIKE setleaf-setname,
*        valsign     LIKE setleaf-valsign,
*        valoption   LIKE setleaf-valoption,
*        valfrom     LIKE setleaf-valfrom,
*        valto       LIKE setleaf-valto,
*       END OF t_set,
*        BEGIN OF t_set2 OCCURS 0,
*        setname     LIKE setleaf-setname,
*        valsign     LIKE setleaf-valsign,
*        valoption   LIKE setleaf-valoption,
*        valfrom     LIKE setleaf-valfrom,
*        valto       LIKE setleaf-valto,
*       END OF t_set2,
*        BEGIN OF t_set3 OCCURS 0,
*        setname     LIKE setleaf-setname,
*        valsign     LIKE setleaf-valsign,
*        valoption   LIKE setleaf-valoption,
*        valfrom     LIKE setleaf-valfrom,
*        valto       LIKE setleaf-valto,
*       END OF t_set3.
*
*   IF st_recibo_pos-kursf < 0.
*  st_recibo_pos-kursf = st_recibo_pos-kursf * - 1.
*ENDIF.
**if ST_RECIBO_POS-HWAE2 = J_1AI02-waers.
**move  '----'    to V_KURSF.
**else.
**write  ST_RECIBO_POS-KURSF    to V_KURSF   RIGHT-JUSTIFIED.
**endif.
*
*IF st_recibo_pos-hwae2 <> 'ARS'.
*  WRITE  st_recibo_pos-kursf    TO v_kursf   RIGHT-JUSTIFIED.
*ELSE.
*  CLEAR v_kursf.
*ENDIF.
*
*WRITE  st_recibo_pos-ret_suss TO v_suss    RIGHT-JUSTIFIED.
*WRITE  st_recibo_pos-ret_iva  TO v_iva     RIGHT-JUSTIFIED.
*WRITE  st_recibo_pos-ret_grav TO v_grav    RIGHT-JUSTIFIED.
*WRITE  st_recibo_pos-ret_iibb TO v_iibb    RIGHT-JUSTIFIED.
*WRITE  st_recibo_pos-hwae2    TO v_hwae2   RIGHT-JUSTIFIED.
*WRITE  st_recibo_pos-buzei    TO v_buzei   RIGHT-JUSTIFIED.
*
*IF V_COBRO = 'X'.
*
*SELECT setname valsign valoption valfrom valto
*      FROM setleaf
*      INTO TABLE t_set
*      WHERE setname = 'ZFI_RECIBO_BANCO'.
*
*SELECT setname valsign valoption valfrom valto
*      FROM setleaf
*      INTO TABLE t_set2
*      WHERE setname = 'ZFI_RECIBO_EFECTIVO'.
*
*SELECT setname valsign valoption valfrom valto
*      FROM setleaf
*      INTO TABLE t_set3
*      WHERE setname = 'ZFI_RECIBO_TRANSFERENCIA'.
*
*  IF V_A_19 = 'X'.
*
*  select belnr buzei WRBTR umskz hkont BSCHL
*  into table t_bseg_tot
*  from bseg
*  where belnr EQ J_1AI02-AUGBL
*    and shkzg NE 'S'
*    and bschl NE '06'
*    and ktosl NE 'WIT'
*    .
*
*    else.
*
*  select belnr buzei WRBTR umskz hkont BSCHL
*  into table t_bseg_tot
*  from bseg
*  where belnr EQ J_1AI02-AUGBL
*    and shkzg EQ 'S'
*    and bschl NE '06'
*    and ktosl NE 'WIT'.
*
*  ENDIF.
*
*  LOOP AT t_bseg_tot into w_bseg_tot.
*
*    IF  w_bseg_tot-umskz = 'V' or w_bseg_tot-umskz EQ 'W'.
*
*      v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*    else.
*
*      read table t_set with key valfrom = w_bseg_tot-hkont.
*        IF sy-subrc = 0.
*
*          v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*        ENDIF.
*
*    read table t_set2 with key valfrom = w_bseg_tot-hkont.
*        IF sy-subrc = 0.
*
*          v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*        ENDIF.
*
*
*     read table t_set3 with key valfrom = w_bseg_tot-hkont.
*        IF sy-subrc = 0.
*
*          v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*        ENDIF.
*
*
*
*    ENDIF.
*
*  ENDLOOP.
*
*IF v_a_19 = 'X'.
*
*    read table t_bseg_tot into w_bseg_tot
*        with key belnr = ST_RECIBO_POS-belnr
*                 buzei = ST_RECIBO_POS-buzei.
*
*    IF sy-subrc = 0.
*
*        IF w_bseg_tot-umskz = 'A' AND w_bseg_tot-bschl = '19' OR w_bseg_tot-bschl = '15'.
*
*           v_dmbtr_v =  v_dmbtr_v  + ST_RECIBO_POS-dmbtr.
*
*        ENDIF.
*
*    ENDIF.
*
*        ENDIF.
*
*ENDIF.
*
*IF v_anticipo = 'X'.
*
*  select belnr buzei WRBTR umskz hkont BSCHL
*  into table t_bseg_tot
*  from bseg
*  where belnr EQ J_1AI02-AUGBL
*    and shkzg EQ 'H'
*    and bschl NE '06'
*    and ktosl NE 'WIT'
*    and umskz EQ 'A'.
*
*   LOOP AT t_bseg_tot into w_bseg_tot.
*
*     v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*  ENDLOOP.
*
*ENDIF.
*
*IF v_factura = 'X'.
*
* select belnr buzei WRBTR umskz hkont BSCHL
*  into table t_bseg_tot
*  from bseg
*  where belnr EQ J_1AI02-AUGBL
*    and shkzg EQ 'S'
*    and bschl NE '06'
*    and ktosl NE 'WIT'.
*
*  v_dmbtr_v =  v_dmbtr_v  + ST_RECIBO_POS-dmbtr.
*
*  IF w_bseg_tot-umskz = 'A' AND w_bseg_tot-bschl = '19'.
*
*           v_dmbtr_v =  v_dmbtr_v  + w_bseg_tot-WRBTR.
*
*  ENDIF.
*
*ENDIF.
*
*select belnr buzei WRBTR umskz hkont BSCHL
*  into table t_bseg_subt
*  from bseg
*  where belnr EQ J_1AI02-AUGBL
*    and umskz NE ' '.
*
**  WRITE  st_recibo_pos-dmbtr    TO v_dmbtr2  RIGHT-JUSTIFIED.
*
*  v_importe_V = v_dmbtr_v.
*
*  LOOP AT t_bseg_subt into w_bseg_subt.
*
*  IF  w_bseg_subt-umskz EQ 'W' OR w_bseg_subt-umskz EQ 'V' OR w_bseg_subt-umskz EQ 'A'.
*
*
*  else.
*
*    v_importe_v =  v_importe_v - w_bseg_subt-WRBTR.
*
*   ENDIF.
*
* ENDLOOP.
*
** WRITE  st_recibo_pos-importe  TO v_importe RIGHT-JUSTIFIED.
*
* WRITE  v_importe_v  TO v_importe RIGHT-JUSTIFIED.
* WRITE  v_importe_v  TO v_dmbtr2 RIGHT-JUSTIFIED.
*
**  v_dmbtr2 =  v_importe_v.
**  v_importe = v_importe_v.
*
*CONDENSE v_kursf   NO-GAPS.
*CONDENSE v_hwae2   NO-GAPS.
*CONDENSE v_importe NO-GAPS.
*CONDENSE v_suss    NO-GAPS.
*CONDENSE v_iva     NO-GAPS.
*CONDENSE v_grav    NO-GAPS.
*CONDENSE v_iibb    NO-GAPS.
*CONDENSE v_buzei   NO-GAPS.
*CONDENSE v_dmbtr2  NO-GAPS.
*
* Vl_WRBTR = v_importe_v + st_recibo_pos-ret_iva + st_recibo_pos-ret_grav + st_recibo_pos-ret_iibb.
*
*write  VL_wrbtr    to V_wrbtr  RIGHT-JUSTIFIED.
*Condense V_wrbtr   no-gaps.
*
* ENDLOOP.
