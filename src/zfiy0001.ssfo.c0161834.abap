
IF st_pago_pos-waers EQ 'ARS'.
  WRITE: 'Pesos'     TO v_waers.
ELSE.
  WRITE: st_pago_pos-waers TO v_waers.
ENDIF.


WRITE: st_pago_pos-bldat    TO v_bldat.

WRITE: st_pago_pos-dmbtr    TO v_dmbtr.
WRITE: st_pago_pos-waers    TO v_waers.
WRITE: st_pago_pos-wt_qbshh TO v_wt_qbshh.

WRITE: st_pago_pos-wrbtr    TO v_wrbtr.
WRITE: st_pago_pos-kursf    TO v_cambio LEFT-JUSTIFIED.
* Incluido 16.11.2012
SHIFT v_cambio LEFT DELETING LEADING '/'.
* Fin 16.11.2012
WRITE: st_pago_pos-waers    TO v_moneda LEFT-JUSTIFIED.

IF st_pago_pos-dmbtr IS INITIAL.

* ---> S4 Migration - 22/06/2023 - FC
*select single wrbtr
*  into st_pago_pos-dmbtr
*  from bseg
*  where BELNR EQ GS_EMISOR-AUGBL
*    and koart EQ 'K'
*    and umskz NE ' '.

  DATA: lt_bseg TYPE fagl_t_bseg.

  CALL FUNCTION 'FAGL_GET_BSEG'
    EXPORTING
      i_belnr = gs_emisor-augbl
      I_BUKRS = gs_emisor-BUKRS "IR180108 dump campo obigatorio
      I_GJAHR = GS_EMISOR-GJAHR "IR180108 dump campo obigatorio
    IMPORTING
      et_bseg = lt_bseg
    EXCEPTIONS
      OTHERS  = 2.

  IF sy-subrc = 0.
    DELETE lt_bseg WHERE koart <> 'K'.
    DELETE lt_bseg WHERE umskz = ' '.
  ENDIF.

  READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

  IF sy-subrc = 0.
    st_pago_pos-dmbtr = wa_bseg-dmbtr.
  ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

  WRITE: st_pago_pos-dmbtr    TO v_dmbtr.

  st_pago_pos-imp_neto = st_pago_pos-imp_neto + st_pago_pos-dmbtr.

ENDIF.

WRITE: st_pago_pos-imp_neto TO v_imp_neto.

CONDENSE v_bldat     NO-GAPS.
CONDENSE v_dmbtr     NO-GAPS.
CONDENSE v_waers     NO-GAPS.
CONDENSE v_wt_qbshh  NO-GAPS.
CONDENSE v_imp_neto  NO-GAPS.

*------> totales
v_wrbtr_t     = v_wrbtr_t    + st_pago_pos-wrbtr.
v_dmbtr_t     = v_dmbtr_t    + st_pago_pos-dmbtr.
v_wt_qbshh_t  = v_wt_qbshh_t + st_pago_pos-wt_qbshh.
v_imp_neto_t  = v_imp_neto_t + st_pago_pos-imp_neto.
